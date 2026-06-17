local M = {}

function M.lyaml()
  local ok, lyaml = pcall(require, "lyaml")
  if ok then
    return lyaml
  end

  vim.notify(
    "rbook.nvim 需要 lyaml 来解析 book.yaml 和文章 front matter。\n请执行：luarocks install lyaml",
    vim.log.levels.ERROR
  )
  return nil
end

function M.snacks()
  local ok, snacks = pcall(require, "snacks")
  if ok then
    return snacks
  end

  vim.notify("rbook.nvim 需要 folke/snacks.nvim。", vim.log.levels.ERROR)
  return nil
end

return M
