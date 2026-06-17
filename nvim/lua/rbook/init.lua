local config = require("rbook.config")

local M = {}

function M.setup(opts)
  -- lazy.nvim 会把 opts 传进这里；命令注册放在 plugin/rbook.lua。
  config.setup(opts)
end

function M.code()
  require("rbook.picker").code()
end

function M.code_files()
  require("rbook.picker").code_files()
end

function M.refresh()
  local data = require("rbook.catalog").refresh()
  if data then
    vim.notify("Rbook 索引已刷新")
  end
end

function M.open_article()
  require("rbook.picker").articles()
end

function M.doctor()
  require("rbook.doctor").run()
end

return M
