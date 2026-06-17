local catalog = require("rbook.catalog")
local paths = require("rbook.paths")

local M = {}

local function add(lines, level, text)
  lines[#lines + 1] = string.format("[%s] %s", level, text)
end

local function code_exists(item)
  return item.code_path and paths.exists(item.code_path)
end

function M.run()
  local data = catalog.refresh()
  if not data then
    return
  end

  local lines = { "# Rbook Doctor", "" }
  local errors = 0
  local warnings = 0

  local required = {
    { "repo_root", paths.repo_root() },
    { "book.yaml", paths.book_yaml() },
    { "pages", paths.pages_root() },
    { "code", paths.code_root() },
  }

  for _, item in ipairs(required) do
    if paths.exists(item[2]) then
      add(lines, "OK", item[1] .. ": " .. item[2])
    else
      errors = errors + 1
      add(lines, "ERROR", item[1] .. " 不存在: " .. item[2])
    end
  end

  for _, err in ipairs(data.errors or {}) do
    errors = errors + 1
    add(lines, "ERROR", err)
  end

  local code_ref_count = {}
  for _, item in ipairs(data.templates) do
    if not code_exists(item) then
      errors = errors + 1
      add(lines, "ERROR", "模板代码不存在: " .. (item.code or "") .. " <- " .. (item.article_route or ""))
    end

    if not item.in_nav and not item.in_glob then
      warnings = warnings + 1
      add(lines, "WARN", "文章未加入 book.yaml 导航或 glob: " .. (item.article_route or ""))
    end

    if item.code_path then
      code_ref_count[item.code_path] = (code_ref_count[item.code_path] or 0) + 1
    end
  end

  for file, count in pairs(code_ref_count) do
    if count > 1 then
      warnings = warnings + 1
      add(lines, "WARN", "同一代码被多个模板引用: " .. paths.relative(file, paths.code_root()))
    end
  end

  lines[#lines + 1] = ""
  lines[#lines + 1] = string.format("templates=%d, code_files=%d, articles=%d, errors=%d, warnings=%d", #data.templates, #data.code_files, #data.articles, errors, warnings)

  vim.cmd("new")
  local buf = vim.api.nvim_get_current_buf()
  vim.api.nvim_buf_set_name(buf, "RbookDoctor")
  vim.bo[buf].buftype = "nofile"
  vim.bo[buf].bufhidden = "wipe"
  vim.bo[buf].filetype = "markdown"
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
end

return M
