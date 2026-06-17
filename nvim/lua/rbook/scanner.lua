local config = require("rbook.config")
local deps = require("rbook.deps")
local paths = require("rbook.paths")

local M = {}

local function read_file(path)
  local lines = vim.fn.readfile(path)
  if not lines then
    return nil
  end
  return table.concat(lines, "\n")
end

local function parse_front_matter(path, lyaml)
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
