-- code 目录下的代码的信息
local M = {}

-- 获取插件根目录
local plugin_root = debug.getinfo(1, "S").source:sub(2):match("(.*/)")

-- 代码片段根目录（相对于插件目录）
M.codeRoot = plugin_root .. '../code/'

-- 用户自定义代码片段目录（可覆盖默认路径）
M.userCodeRoot = vim.fn.stdpath('data') .. '/rbook/code/'

-- 获取代码片段的详细信息和描述
local function get_snippet_info(filename)
  local info = {
    name = filename,
    description = "",
    category = "",
    tags = {}
  }
  
  -- 根据文件名推断类别和描述
  if filename:match("template") then
    info.category = "template"
    info.description = "代码模板"
  elseif filename:match("cpp$") then
    info.category = "cpp"
    info.description = "C++ 代码片段"
  elseif filename:match("py$") then
    info.category = "python"
    info.description = "Python 代码片段"
  elseif filename:match("lua$") then
    info.category = "lua"
    info.description = "Lua 代码片段"
  elseif filename:match("java$") then
    info.category = "java"
    info.description = "Java 代码片段"
  end
  
  return info
end

-- 获取所有代码片段的详细信息
function M.get_all_snippets()
  local snippets = {}
  local handle = io.popen("find " .. M.codeRoot .. " -type f -printf '%P\\n'")
  if not handle then return snippets end
  
  for line in handle:lines() do
    local info = get_snippet_info(line)
    info.full_path = M.codeRoot .. line
    table.insert(snippets, info)
  end
  handle:close()
  
  return snippets
end

-- 根据名称搜索代码片段
function M.search_snippets(query)
  local all_snippets = M.get_all_snippets()
  local results = {}
  
  for _, snippet in ipairs(all_snippets) do
    if snippet.name:lower():find(query:lower(), 1, true) or
       snippet.description:lower():find(query:lower(), 1, true) or
       snippet.category:lower():find(query:lower(), 1, true) then
      table.insert(results, snippet)
    end
  end
  
  return results
end

-- 根据分类获取代码片段
function M.get_snippets_by_category(category)
  local all_snippets = M.get_all_snippets()
  local results = {}
  
  for _, snippet in ipairs(all_snippets) do
    if snippet.category == category then
      table.insert(results, snippet)
    end
  end
  
  return results
end

-- 读取代码片段内容
function M.read_snippet_content(path)
  local file = io.open(path, "r")
  if not file then return {} end
  
  local lines = {}
  for line in file:lines() do
    table.insert(lines, line)
  end
  file:close()
  
  return lines
end

-- 获取代码片段的预览内容
function M.get_snippet_preview(path, max_lines)
  max_lines = max_lines or 10
  local lines = M.read_snippet_content(path)
  local preview = {}
  
  for i = 1, math.min(#lines, max_lines) do
    table.insert(preview, lines[i])
  end
  
  if #lines > max_lines then
    table.insert(preview, "...")
  end
  
  return preview
end

-- 更新代码片段信息缓存
function M.refresh_snippets()
  -- 清除缓存并重新获取
  return M.get_all_snippets()
end

return M