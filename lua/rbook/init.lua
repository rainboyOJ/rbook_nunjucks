local Snacks = require("snacks")
local M = {}

-- 获取插件根目录（用于lazy.nvim等插件管理器）
local plugin_root = debug.getinfo(1, "S").source:sub(2):match("(.*/)")
local rbook_root = plugin_root .. '../../'

-- local RbookCode = load_rbook_code()

local RbookCode = require("rbook.rbook_code")


-- 插件内置代码片段路径（相对于插件目录）
M.builtinCodePath = rbook_root .. 'code/'

-- 获取 snippets 列表 - 已删除，只使用 code 目录

-- 读取 snippet 内容
local function read_snippet_content(path)
  local lines = vim.fn.readfile(path)
  return lines
end

-- 获取文件预览
local function get_file_preview(file_path)
  -- 检查文件是否存在
  if vim.fn.filereadable(file_path) == 0 then
    return "File not found: " .. file_path
  end
  
  -- 读取文件内容
  local lines = vim.fn.readfile(file_path)
  if not lines or #lines == 0 then
    return "Empty file: " .. file_path
  end
  
  -- 限制预览行数，最多显示前20行
  local max_lines = 20
  local preview_lines = {}
  
  for i = 1, math.min(#lines, max_lines) do
    table.insert(preview_lines, lines[i])
  end
  
  -- 如果文件超过20行，添加省略提示
  if #lines > max_lines then
    table.insert(preview_lines, "... (" .. (#lines - max_lines) .. " more lines)")
  end
  
  return table.concat(preview_lines, "\n")
end

-- 可以根据 picker 状态变化的格式化器
local function code_snip_format(item, picker)
  local ret = {}
  
  local is_selected = picker.list:is_selected(item)
  
  if is_selected then
    ret[#ret + 1] = { "✓ ", "Special" }
  else
    ret[#ret + 1] = { "  ", "Normal" }
  end
  
  ret[#ret + 1] = {item.name, is_selected and "Special" or "Normal" }
  -- ret[#ret + 1] = {item.path }
  -- -- print(vim.inspect(item))
  
  -- -- 虚拟文本示例（显示在行尾）
  return ret
end

-- 插入代码片段 - 已删除，只使用 code 目录的 insert_code_snippet_from_code

-- 插入代码片段（从code目录）
local function insert_code_snippet_from_code(snippet_info)
  local lines = RbookCode.read_snippet_content(snippet_info.full_path)
  local filename = snippet_info.name

  -- 添加折叠标记
  table.insert(lines, 1, "//oisnip_begin" .. filename)
  table.insert(lines, "//oisnip_end")

  -- 获取当前缓冲区和窗口
  local bufnr = vim.api.nvim_get_current_buf()
  local cur_pos = vim.api.nvim_win_get_cursor(0)
  
  -- 插入代码片段
  vim.api.nvim_buf_set_lines(bufnr, cur_pos[1] - 1, cur_pos[1], false, lines)
end

-- 从oiSnippets选择插入 - 已删除，只使用 code 目录

-- 从code目录选择插入
function InsertCodeSnippet()
  local snippets = RbookCode
  
  local items = {}
  for _, snippet in ipairs(snippets) do
    local full_path = rbook_root .. snippet.path
    table.insert(items, {
      name = snippet.name,
      file = full_path,
      info = snippet,
      preview = get_file_preview(full_path),
      display = string.format("%s (%s) - %s", 
        snippet.name, snippet.category, snippet.description)
    })
  end
  
  Snacks.picker.pick({
    items = items,
    format = code_snip_format,
    prompt = "Select Code Snippet:",
    confirm = function(picker, item)
      picker:norm(function()
        if item then
          picker:close()
          insert_code_snippet_from_code(item.info)
        end
      end)
    end
  })
end

-- 搜索代码片段
function SearchCodeSnippets()
  vim.ui.input({ prompt = "Search code snippets: " }, function(query)
    if not query or query == "" then return end
    
    local results = RbookCode.search_snippets(query)
    
    local items = {}
    for _, snippet in ipairs(results) do
      table.insert(items, {
        name = snippet.name,
        file = snippet.full_path,
        info = snippet,
        preview = RbookCode.get_snippet_preview(snippet.full_path),
        display = string.format("%s (%s) - %s", 
          snippet.name, snippet.category, snippet.description)
      })
    end
    
    if #items == 0 then
      vim.notify("No snippets found for: " .. query, vim.log.levels.INFO)
      return
    end
    
    Snacks.picker.pick({
      items = items,
      format = "file",
      prompt = "Search Results:",
      confirm = function(picker, item)
        picker:norm(function()
          if item then
            picker:close()
            insert_code_snippet_from_code(item.info)
          end
        end)
      end
    })
  end)
end

-- 按分类浏览代码片段
function BrowseCodeSnippetsByCategory()
  local categories = {}
  local snippets = RbookCode.get_all_snippets()
  
  -- 收集所有分类
  for _, snippet in ipairs(snippets) do
    if not categories[snippet.category] then
      categories[snippet.category] = {}
    end
    table.insert(categories[snippet.category], snippet)
  end
  
  local category_items = {}
  for category, _ in pairs(categories) do
    table.insert(category_items, {
      name = category,
      display = category .. " (" .. #categories[category] .. " snippets)",
      category = category
    })
  end
  
  Snacks.picker.pick({
    items = category_items,
    prompt = "Select Category:",
    confirm = function(picker, item)
      picker:norm(function()
        if item then
          picker:close()
          
          -- 显示该分类下的所有代码片段
          local snippets_in_category = RbookCode.get_snippets_by_category(item.category)
          local items = {}
          
          for _, snippet in ipairs(snippets_in_category) do
            table.insert(items, {
              name = snippet.name,
              file = snippet.full_path,
              info = snippet,
              preview = RbookCode.get_snippet_preview(snippet.full_path),
              display = snippet.name .. " - " .. snippet.description
            })
          end
          
          Snacks.picker.pick({
            items = items,
            format = "file",
            prompt = item.category .. " Snippets:",
            confirm = function(picker2, item2)
              picker2:norm(function()
                if item2 then
                  picker2:close()
                  insert_code_snippet_from_code(item2.info)
                end
              end)
            end
          })
        end
      end)
    end
  })
end

function M.setup(opts)
  opts = opts or {}

  -- 设置路径（只保留 codePath）
  M.codePath = opts.codePath or M.codePath

  -- 确保路径存在
  if not vim.fn.isdirectory(M.codePath) then
    vim.fn.mkdir(M.codePath, "p")
  end

  -- 创建命令（只保留 code 相关的命令）
  vim.api.nvim_create_user_command("OICodeSnip", function()
    InsertCodeSnippet()
  end, { desc = "Insert code snippet from code directory" })
  
end

return M
