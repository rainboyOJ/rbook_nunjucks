local M = {}

-- 插件只保存轻量配置；真正扫描 book/code 的动作延迟到第一次命令执行。
M.defaults = {
  repo_root = nil,
  insert = {
    -- 默认插入纯代码，避免把额外注释带进 OJ 提交。
    fold_markers = false,
  },
  files = {
    -- Rbook 的 code 目录以 C++ 为主，也允许少量脚本和说明文件出现在兜底搜索里。
    extensions = { "cpp", "cc", "cxx", "c", "h", "hpp", "py", "sh", "md", "hs" },
  },
  picker = {
    preview = true,
  },
}

M.options = vim.deepcopy(M.defaults)

local function merge(defaults, opts)
  return vim.tbl_deep_extend("force", defaults, opts or {})
end

function M.setup(opts)
  M.options = merge(vim.deepcopy(M.defaults), opts)
  return M.options
end

function M.get()
  return M.options
end

return M
