local config = require("rbook.config")
local deps = require("rbook.deps")
local paths = require("rbook.paths")

local M = {}

-- scanner 是插件的数据入口：
-- 1. 读取 book.yaml，得到导航和 glob；
-- 2. 扫描 book/pages 的 front matter，抽取正式 code_template；
-- 3. 扫描 book/code，作为 RbookCodeFiles 的兜底文件列表。

local function read_file(path)
  local lines = vim.fn.readfile(path)
  if not lines then
    return nil
  end
  return table.concat(lines, "\n")
end

local function parse_front_matter(path, lyaml)
  -- 只解析 Markdown 文件顶部的 front matter；正文不进入索引。
  -- 这样搜索足够快，也能保持插件职责清楚：找模板，不做全文检索。
  local content = read_file(path)
  if not content or not content:match("^%-%-%-") then
    return nil
  end

  local yaml_text = content:match("^%-%-%-\n(.-)\n%-%-%-")
  if not yaml_text then
    return nil
  end

  local ok, data = pcall(lyaml.load, yaml_text)
  if not ok or type(data) ~= "table" then
    return nil, data
  end

  return data
end

local function iter_files(root, predicate)
  -- 使用 libuv 递归扫描，避免依赖外部 find/rg 命令。
  local result = {}
  if not paths.exists(root) then
    return result
  end

  local function walk(dir)
    local handle = vim.uv.fs_scandir(dir)
    if not handle then
      return
    end

    while true do
      local name, t = vim.uv.fs_scandir_next(handle)
      if not name then
        break
      end
      local full = dir .. "/" .. name
      if t == "directory" then
        walk(full)
      elseif t == "file" and predicate(full) then
        result[#result + 1] = full
      end
    end
  end

  walk(root)
  table.sort(result)
  return result
end

local function list_contains(list, value)
  if type(list) ~= "table" then
    return false
  end
  for _, item in ipairs(list) do
    if item == value then
      return true
    end
  end
  return false
end

local function join_book_path(base, path)
  -- book.yaml 的章节 path 是相对父章节的。
  -- 例如 graph 章节下的 mst/最小生成树.md，真实文章 route 是 graph/mst/最小生成树.md。
  if not path or path == "" or path == "." then
    return base
  end

  if path:match("^/") then
    return path:gsub("^/", "")
  end

  if not base or base == "" or base == "." then
    return path
  end

  return (base .. "/" .. path):gsub("//+", "/"):gsub("^%./", "")
end

local function path_is_page(path)
  return path and path:match("%.md$")
end

local function scan_nav_node(node, section, nav, base)
  -- 把 book.yaml 的章节树压平成 path -> { title, section }。
  -- 这个结果只用于给模板补充分类信息，以及 doctor 判断文章是否挂到首页。
  if type(node) ~= "table" then
    return
  end

  local full_path = node.path and join_book_path(base, node.path) or nil
  if node.title and node.path then
    nav[full_path] = {
      title = node.title,
      section = section,
    }
  end

  local next_section = section
  if node.title and not node.path then
    next_section = node.title
  end

  local next_base = base
  if node.path and not path_is_page(node.path) then
    next_base = full_path
  end

  for _, value in pairs(node) do
    if type(value) == "table" then
      scan_nav_node(value, next_section, nav, next_base)
    end
  end
end

local function scan_book_yaml(lyaml)
  local file = paths.book_yaml()
  if not paths.exists(file) then
    return {}, {}, "book.yaml 不存在: " .. file
  end

  local ok, data = pcall(lyaml.load, read_file(file))
  if not ok or type(data) ~= "table" then
    return {}, {}, "book.yaml 解析失败"
  end

  local nav = {}
  scan_nav_node(data.chapters, nil, nav, "")

  local glob = {}
  if type(data.glob) == "table" then
    for _, item in ipairs(data.glob) do
      glob[item] = true
    end
  end

  return nav, glob, nil
end

local function article_nav_key(article_path)
  -- book.yaml 里常用目录路径表示 index.md 页面：
  -- graph/save 等价于 graph/save/index.md。
  local rel = paths.article_route(article_path)
  if not rel then
    return nil
  end
  if rel:match("/index%.md$") then
    return rel:gsub("/index%.md$", "")
  end
  return rel
end

local function normalize_tags(value)
  if type(value) == "table" then
    return value
  end
  if type(value) == "string" and value ~= "" then
    return { value }
  end
  return {}
end

local function make_template(article_path, fm, item, nav, glob)
  -- 一个 code_template 项最终会变成 picker 的一条记录。
  -- 这里把文章信息、代码路径、导航状态都合并好，后续 UI 不再理解 YAML 结构。
  local code_abs = paths.resolve_code_path(item.code, article_path)
  local article_key = article_nav_key(article_path)
  local route = paths.article_route(article_path)
  local nav_info = nav[article_key] or nav[route]
  local in_glob = glob[route] or glob[article_key]

  return {
    title = item.title or fm.title or vim.fn.fnamemodify(code_abs or "", ":t"),
    desc = item.desc or fm.description or "",
    tags = normalize_tags(item.tags or fm.tags),
    categories = normalize_tags(fm.categories),
    section = nav_info and nav_info.section or nil,
    article_title = fm.title,
    article_path = article_path,
    article_route = route,
    code = item.code,
    code_path = code_abs,
    code_url = code_abs and paths.code_url(code_abs) or nil,
    in_nav = nav_info ~= nil,
    in_glob = in_glob == true,
    source = "code_template",
  }
end

local function scan_templates(lyaml, nav, glob)
  -- 正式模板只来自文章 front matter 的 code_template。
  -- 这样主列表不会被 book/code 里的临时代码和旧代码淹没。
  local articles = {}
  local templates = {}
  local errors = {}

  local files = iter_files(paths.pages_root(), function(path)
    return path:match("%.md$")
  end)

  for _, file in ipairs(files) do
    local fm, err = parse_front_matter(file, lyaml)
    if err then
      errors[#errors + 1] = "front matter 解析失败: " .. file
    end

    if fm then
      articles[#articles + 1] = {
        title = fm.title or paths.article_route(file),
        tags = normalize_tags(fm.tags),
        categories = normalize_tags(fm.categories),
        path = file,
        route = paths.article_route(file),
        in_nav = (nav[article_nav_key(file)] or nav[paths.article_route(file)]) ~= nil,
      }

      if type(fm.code_template) == "table" then
        for _, item in ipairs(fm.code_template) do
          if type(item) == "table" and item.code then
            templates[#templates + 1] = make_template(file, fm, item, nav, glob)
          end
        end
      end
    end
  end

  return articles, templates, errors
end

local function file_allowed(path)
  local ext = path:match("%.([%w_%-]+)$")
  if not ext then
    return false
  end
  return list_contains(config.get().files.extensions, ext)
end

local function scan_code_files()
  -- 兜底列表：直接浏览 book/code 下允许扩展名的文件。
  -- 它不代表“正式模板”，只是方便写题时临时查找。
  local files = iter_files(paths.code_root(), file_allowed)
  local result = {}

  for _, file in ipairs(files) do
    local rel = paths.relative(file, paths.code_root())
    result[#result + 1] = {
      title = vim.fn.fnamemodify(file, ":t"),
      desc = rel,
      tags = {},
      categories = {},
      code_path = file,
      code_url = "/code/" .. rel,
      source = "code_file",
      is_markdown = file:match("%.md$") ~= nil,
    }
  end

  return result
end

function M.scan()
  -- 统一返回 catalog.lua 缓存需要的全部数据。
  -- 缺依赖时返回 nil，让上层命令自然停止。
  local lyaml = deps.lyaml()
  if not lyaml then
    return nil
  end

  local nav, glob, nav_error = scan_book_yaml(lyaml)
  local articles, templates, template_errors = scan_templates(lyaml, nav, glob)

  local errors = {}
  if nav_error then
    errors[#errors + 1] = nav_error
  end
  vim.list_extend(errors, template_errors)

  return {
    templates = templates,
    code_files = scan_code_files(),
    articles = articles,
    nav = nav,
    glob = glob,
    errors = errors,
    scanned_at = os.time(),
  }
end

return M
