local scanner = require("rbook.scanner")

local M = {
  current = nil,
}

function M.refresh()
  M.current = scanner.scan()
  return M.current
end

function M.get()
  if not M.current then
    return M.refresh()
  end
  return M.current
end

return M
