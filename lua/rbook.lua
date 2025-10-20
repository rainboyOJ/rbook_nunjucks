local Snacks = require("snacks")
local RbookCode = require("rbook_code")
local M = {}

-- 获取插件根目录（用于lazy.nvim等插件管理器）
local plugin_root = debug.getinfo(1, "S").source:sub(2):match("(.*/)")
local rbook_root = plugin_root .. '../'
local RbookCode = require("rbook_code")

-- 插件内置代码片段路径（相对于插件目录）
M.builtinCodePath = rbook_root .. 'code/'

-- 获取 snippets 列表
local function get_snippet_list()
  local handle = io.popen("find " .. M.snippetPath .. " -type f -printf '%P\\n'")
  if not handle then return {} end
  
  local snippets = {}
  for line in handle:lines() do
    table.insert(snippets, line)
  end
  handle:close()
  return snippets
end

-- 读取 snippet 内容
local function read_snippet_content(path)
  local lines = vim.fn.readfile(path)
  return lines
end

-- 插入代码片段
local function insert_code_snippet(snip_path)
  local lines = read_snippet_content(snip_path)
  local filename = vim.fn.fnamemodify(snip_path, ":t")

  -- 特殊处理模板文件
  if filename == "simaple_template.cpp" or filename:find("template") then
    -- 替换日期
    local date = os.date("%Y-%m-%d %H:%M:%S")
    for i, line in ipairs(lines) do
      lines[i] = line:gsub("2025%-10%-02 10:34:43", date)
    end
  else
    -- 添加折叠标记到非模板文件
    table.insert(lines, 1, "//oisnip_begin" .. filename)
    table.insert(lines, "//oisnip_end")
  end

  -- 获取当前缓冲区和窗口
  local bufnr = vim.api.nvim_get_current_buf()
  local cur_pos = vim.api.nvim_win_get_cursor(0)
  
  -- 插入代码片段
  vim.api.nvim_buf_set_lines(bufnr, cur_pos[1] - 1, cur_pos[1], false, lines)

  -- 特殊处理后的光标定位
  if filename == "simaple_template.cpp" then
    -- 折叠代码片段
    vim.cmd("normal! zM")
    -- 移动光标到init函数
    for i, line in ipairs(lines) do
      if line:find("void init") then
        vim.api.nvim_win_set_cursor(0, {i+1, 4})
        vim.cmd("normal! zz")
        vim.api.nvim_input("<c-y>")
        vim.api.nvim_input("<c-y>")
        break
      end
    end
  end
end

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

-- 从oiSnippets选择插入
function InsertSnippet()
  Snacks.picker.pick("files", {
    dirs = {M.snippetPath},
    hidden = true,
    cwd = M.snippetPath,
    prompt = "Select Snippet:",
    confirm = function(picker, item)
      picker:norm(function()
        if item then
          picker:close()
          insert_code_snippet(item.file)
        end
      end)
    end
  })
end

-- 从code目录选择插入
function InsertCodeSnippet()
  local snippets = RbookCode.get_all_snippets()
  
  local items = {}
  for _, snippet in ipairs(snippets) do
    table.insert(items, {
      name = snippet.name,
      file = snippet.full_path,
      info = snippet,
      preview = RbookCode.get_snippet_preview(snippet.full_path),
      display = string.format("%s (%s) - %s", 
        snippet.name, snippet.category, snippet.description)
    })
  end
  
  Snacks.picker.pick({
    items = items,
    format = "file",
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

  -- 设置路径
  M.snippetPath = opts.snippetPath or M.snippetPath
  M.codePath = opts.codePath or M.codePath

  -- 确保路径存在
  if not vim.fn.isdirectory(M.snippetPath) then
    vim.fn.mkdir(M.snippetPath, "p")
  end
  if not vim.fn.isdirectory(M.codePath) then
    vim.fn.mkdir(M.codePath, "p")
  end

  -- 创建命令
  vim.api.nvim_create_user_command("OISnipChoose", function()
    InsertSnippet()
  end, { desc = "Insert snippet from oiSnippets" })
  
  vim.api.nvim_create_user_command("OICodeSnip", function()
    InsertCodeSnippet()
  end, { desc = "Insert code snippet from code directory" })
  
  vim.api.nvim_create_user_command("OISearchSnip", function()
    SearchCodeSnippets()
  end, { desc = "Search code snippets" })
  
  vim.api.nvim_create_user_command("OIBrowseSnip", function()
    BrowseCodeSnippetsByCategory()
  end, { desc = "Browse code snippets by category" })

  -- 设置快捷键
  vim.keymap.set('n', '<leader>os', ":OISnipChoose<CR>", { 
    buffer = true, silent = true, desc = "oiSnippets" 
  })
  
  vim.keymap.set('n', '<leader>oc', ":OICodeSnip<CR>", { 
    buffer = true, silent = true, desc = "Code Snippets" 
  })
  
  vim.keymap.set('n', '<leader>of', ":OISearchSnip<CR>", { 
    buffer = true, silent = true, desc = "Search Snippets" 
  })
  
  vim.keymap.set('n', '<leader>ob', ":OIBrowseSnip<CR>", { 
    buffer = true, silent = true, desc = "Browse Snippets by Category" 
  })
end

return M
