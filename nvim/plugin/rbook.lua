if vim.g.loaded_rbook_nvim then
  return
end
vim.g.loaded_rbook_nvim = true

local function rbook()
  return require("rbook")
end

vim.api.nvim_create_user_command("RbookCode", function()
  rbook().code()
end, { desc = "搜索并插入 rbook 正式代码模板" })

vim.api.nvim_create_user_command("RbookCodeFiles", function()
  rbook().code_files()
end, { desc = "浏览并插入 book/code 下的代码文件" })

vim.api.nvim_create_user_command("RbookCodeRefresh", function()
  rbook().refresh()
end, { desc = "刷新 rbook 本地索引" })

vim.api.nvim_create_user_command("RbookOpenArticle", function()
  rbook().open_article()
end, { desc = "搜索并打开 rbook 文章源文件" })

vim.api.nvim_create_user_command("RbookDoctor", function()
  rbook().doctor()
end, { desc = "检查 rbook 文章、模板和导航的一致性" })
