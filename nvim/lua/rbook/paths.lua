local config = require("rbook.config")

local M = {}

local function normalize(path)
  if not path or path == "" then
    return nil
  end
  local normalized = vim.fn.fnamemodify(path, ":p"):gsub("/$", "")
  return vim.fs.normalize(normalized)
end

local function plugin_repo_root()
  -- 本插件目录是 repo_root/nvim/lua/rbook，所以向上三层回到仓库根。
  -- 用户显式传 repo_root 时不会走这个兜底逻辑。
  local source = debug.getinfo(1, "S").source:sub(2)
  local lua_file = vim.fs.normalize(source)
  return normalize(vim.fs.dirname(lua_file) .. "/../../..")
end

function M.repo_root()
  local opts = config.get()
  return normalize(opts.repo_root) or plugin_repo_root()
end

function M.book_root()
  return M.repo_root() .. "/book"
end

function M.pages_root()
  return M.book_root() .. "/pages"
end

function M.code_root()
  return M.book_root() .. "/code"
end

function M.book_yaml()
  return M.book_root() .. "/book.yaml"
end

function M.join(...)
  return vim.fs.normalize(table.concat({ ... }, "/"))
end

function M.exists(path)
  return path and vim.uv.fs_stat(path) ~= nil
end

function M.relative(path, root)
  path = vim.fs.normalize(path)
  root = vim.fs.normalize(root)
  root = root:gsub("/$", "")
  if path:sub(1, #root + 1) == root .. "/" then
    return path:sub(#root + 2)
  end
  return path
end

function M.resolve_code_path(code_path, article_path)
  if not code_path or code_path == "" then
    return nil
  end

  if code_path:match("^/code/") then
    -- 文章 front matter 推荐写 /code/...，这是电子书里的 Web 路径。
    -- 插件运行在本地，需要映射回 repo_root/book/code/...。
    return M.code_root() .. "/" .. code_path:gsub("^/code/", "")
  end

  if code_path:match("^code/") then
    return M.book_root() .. "/" .. code_path
  end

  if code_path:match("^/") then
    return code_path
  end

  if article_path then
    -- 兼容少量旧文章里的 ./template.cpp 之类相对路径。
    return M.join(vim.fs.dirname(article_path), code_path)
  end

  return M.join(M.book_root(), code_path)
end

function M.code_url(abs_path)
  local rel = M.relative(abs_path, M.code_root())
  if rel:match("^%.%.") or rel:match("^/") then
    return nil
  end
  return "/code/" .. rel
end

function M.article_route(abs_path)
  local rel = M.relative(abs_path, M.pages_root())
  if rel:match("^%.%.") or rel:match("^/") then
    return nil
  end
  return rel
end

return M
