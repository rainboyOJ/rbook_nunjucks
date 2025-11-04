local Snacks = require("snacks")
local M = {}

-- 获取插件根目录（用于lazy.nvim等插件管理器）
local plugin_root = debug.getinfo(1, "S").source:sub(2):match("(.*/)")
local rbook_root = plugin_root .. '../../'

-- 设置全局变量 rbook_root
_G.RbookRoot = rbook_root

-- local RbookCode = load_rbook_code()

local RbookCode = require("rbook.rbook_code")


-- 插件内置代码片段路径（相对于插件目录）
M.builtinCodePath = rbook_root .. 'code/'

-- 获取 snippets 列表 - 已删除，只使用 code 目录


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
  
  ret[#ret + 1] = {item.text or 'Unknown' , is_selected and "Special" or "Normal" }
  -- ret[#ret + 1] = {item.path }
  -- -- print(vim.inspect(item))
  
  -- -- 虚拟文本示例（显示在行尾）
  return ret
end

-- 插入代码片段 - 已删除，只使用 code 目录的 insert_code_snippet_from_code

-- 插入代码片段（从code目录）
local function insert_code_snippet_from_code(snippet_info)
  local lines =  vim.fn.readfile(rbook_root .. snippet_info.path)
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

local function format_snip_name(snip)
  local tags = ""
  -- format tags as `[tag1][tag1][...]
  for _,tag in ipairs(snip.tags) do
    tags = tags .. "[" .. tag .. "]"
  end
  return tags .. snip.name
end


-- 从oiSnippets选择插入 - 已删除，只使用 code 目录

-- 从code目录选择插入
function InsertCodeSnippet()
  local snippets = RbookCode

  local items = {}
  local idx = 0
  for _, snippet in ipairs(snippets) do
    idx = idx + 1
    local full_path = rbook_root .. snippet.path
    table.insert(items, {
      id = idx,
      text = format_snip_name(snippet),
      file = full_path,
      info = snippet,
      display = string.format("%s (%s) - %s",
        snippet.name, snippet.category, snippet.description),
      snippet = snippet -- 保存整个snippet信息
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
          insert_code_snippet_from_code(item.snippet)
        end
      end)
    end
  })
end

-- 插入文件内容的辅助函数
local function insert_file_content(picker, file_path)
  -- 检查文件是否存在
  if vim.fn.filereadable(file_path) == 0 then
    Snacks.notify.warn("File is not readable: " .. file_path)
    return
  end

  -- 读取文件内容
  local lines = {}
  local file = io.open(file_path, "r")
  if not file then
    Snacks.notify.error("Failed to read file: " .. file_path)
    return
  end

  -- 读取所有行
  for line in file:lines() do
    table.insert(lines, line)
  end
  file:close()

  -- 获取当前光标位置
  local win = vim.api.nvim_get_current_win()
  local buf = vim.api.nvim_win_get_buf(win)
  local row, col = unpack(vim.api.nvim_win_get_cursor(win))

  -- 插入内容到当前 buffer
  vim.api.nvim_buf_set_lines(buf, row - 1, row - 1, false, lines)

  -- 可选：移动光标到插入内容的开始位置
  -- vim.api.nvim_win_set_cursor(win, { row, col })

  -- 关闭 picker
  picker:close()

  Snacks.notify.info("Inserted content from: " .. vim.fn.fnamemodify(file_path, ":t"))
end

local win -- 保存当前窗口和光标位置

local insert_file_content = function (file_path)
  -- 检查文件是否存在
  if vim.fn.filereadable(file_path) == 0 then
    Snacks.notify.warn("File is not readable: " .. file_path)
    return
  end

  -- 读取文件内容
  local lines = vim.fn.readfile(file_path)
  if not lines then
    Snacks.notify.error("Failed to read file: " .. file_path)
    return
  end

  -- 确保 win, buf, row 变量是有效的
  if not win then
    Snacks.notify.error("Target window details not found. Cannot insert content.")
    return
  end

  -- 插入内容到 win 对应的 buffer 的光标处
  local row ,col = unpack(vim.api.nvim_win_get_cursor(win))
  local buf = vim.api.nvim_win_get_buf(win)
  vim.api.nvim_buf_set_lines(buf, row - 1, row - 1, false, lines)
  
  Snacks.notify.info("Inserted content from: " .. vim.fn.fnamemodify(file_path, ":t"))
end

local function my_action_for_explorer_insert_code(picker,item)
  vim.schedule(function() insert_file_content(item.file) end)
end

local myexplorer_config = {
  finder = "explorer",
  cwd =  M.builtinCodePath,
  sort = { fields = { "sort" } },
  supports_live = true,
  tree = true,
  watch = false,
  diagnostics = false,
  diagnostics_open = false,
  git_status = false,
  git_status_open = false,
  git_untracked = false,
  follow_file = true,
  focus = "list",
  auto_close = false,
  jump = { close = false },
  -- layout = { preset = "sidebar", preview = true},
  -- to show the explorer to the right, add the below to
  -- your config under `opts.picker.sources.explorer`
  layout = { layout = { position = "right" } },
  formatters = {
    file = { filename_only = true },
    severity = { pos = "right" },
  },
  actions = {
    myedit = function (picker,item)
      vim.schedule(function() insert_file_content(item.file) end)
    end
  },
  win = {
    list = {
      keys = {
        ["<BS>"] = "explorer_up",
        ["l"] = "confirm",
        ["h"] = "explorer_close", -- close directory
        -- ["a"] = "explorer_add",
        ["o"] = "explorer_open",
        ["c"] = "explorer_close",
        ["p"] = "toggle_preview",
        ["a"] = "myedit",
      },
    },
  },
}



local function mypick()
  -- 得到当前的窗口
  win = vim.api.nvim_get_current_win()
  Snacks.picker.explorer(myexplorer_config)
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

  vim.api.nvim_create_user_command("OICodeSnipPick", function()
    win = vim.api.nvim_get_current_win() -- 保存当前窗口
    Snacks.picker.explorer(myexplorer_config)
  end, { desc = "Insert code snippet from code directory" })


  local TemplateEngine = require("rbook.template_engine")
  TemplateEngine.create_commands()

  -- 应用C++模板命令
  vim.api.nvim_create_user_command('ApplyTempCpp', function()
    local template_path = rbook_root .. 'code/template/template.cpp'
    -- TemplateEngine.apply_template_to_buffer(template_path)
    vim.api.nvim_cmd({ cmd = 'TemplateApply', args = { template_path } }, {})
  end, { desc = "应用C++模板" })
end

return M
