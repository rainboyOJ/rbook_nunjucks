local config = require("rbook.config")
local deps = require("rbook.deps")
local paths = require("rbook.paths")

local M = {}

local function notify(message, level)
  local snacks = deps.snacks()
  if snacks and snacks.notify then
    snacks.notify(message, { level = level or vim.log.levels.INFO })
  else
    vim.notify(message, level or vim.log.levels.INFO)
  end
end

local function read_lines(path)
  if vim.fn.filereadable(path) == 0 then
    notify("文件不可读: " .. path, vim.log.levels.ERROR)
    return nil
  end
  return vim.fn.readfile(path)
end

local function with_fold_markers(lines, item)
  if not config.get().insert.fold_markers then
    return lines
  end

  local title = item.title or vim.fn.fnamemodify(item.code_path, ":t")
  local copy = vim.list_slice(lines)
  table.insert(copy, 1, "// rbook_begin: " .. title)
  copy[#copy + 1] = "// rbook_end"
  return copy
end

function M.insert_code(item, opts)
  opts = opts or {}
  if not item or not item.code_path then
    notify("没有可插入的代码文件", vim.log.levels.ERROR)
    return
  end

  local lines = read_lines(item.code_path)
  if not lines then
    return
  end

  if opts.fold_markers then
    local old = config.get().insert.fold_markers
    config.get().insert.fold_markers = true
    lines = with_fold_markers(lines, item)
    config.get().insert.fold_markers = old
  else
    lines = with_fold_markers(lines, item)
  end

  local win = vim.api.nvim_get_current_win()
  local buf = vim.api.nvim_win_get_buf(win)
  local row = vim.api.nvim_win_get_cursor(win)[1]
  vim.api.nvim_buf_set_lines(buf, row - 1, row - 1, false, lines)
  notify("已插入: " .. vim.fn.fnamemodify(item.code_path, ":t"))
end

function M.copy_code(item)
  if not item or not item.code_path then
    notify("没有可复制的代码文件", vim.log.levels.ERROR)
    return
  end

  local lines = read_lines(item.code_path)
  if not lines then
    return
  end

  vim.fn.setreg("+", table.concat(lines, "\n"))
  vim.fn.setreg('"', table.concat(lines, "\n"))
  notify("已复制: " .. vim.fn.fnamemodify(item.code_path, ":t"))
end

function M.open_code(item)
  if item and item.code_path then
    vim.cmd.edit(vim.fn.fnameescape(item.code_path))
  end
end

function M.open_article(item)
  if item and item.article_path then
    vim.cmd.edit(vim.fn.fnameescape(item.article_path))
  elseif item and item.path then
    vim.cmd.edit(vim.fn.fnameescape(item.path))
  end
end

function M.open_code_root()
  vim.cmd.edit(vim.fn.fnameescape(paths.code_root()))
end

return M
