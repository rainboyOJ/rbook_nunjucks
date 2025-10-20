# rbook.nvim 插件结构与路径配置

## 📦 插件目录结构（适用于 lazy.nvim）

### 标准插件结构
```
rbook.nvim/
├── lua/
│   ├── rbook.lua              # 主插件文件
│   ├── rbook_code.lua         # 代码片段管理模块
│   └── (其他模块...)
├── code/                      # 内置代码片段
│   ├── algorithms/
│   ├── templates/
│   └── ...
├── oiSnippets/                # 内置代码片段
│   ├── cpp/
│   ├── python/
│   └── ...
├── README.md
├── LICENSE
└── (其他文档...)
```

### 用户数据结构
```
~/.local/share/nvim/rbook/     # vim.fn.stdpath('data')/rbook/
├── code/                      # 用户代码片段
│   ├── my_algorithms/
│   ├── personal_templates/
│   └── ...
└── oiSnippets/                # 用户片段
    ├── quick_snippets/
    └── ...
```

## 🎯 路径配置详解

### 1. 插件安装方式对比

#### 方式一：直接复制到 config 目录（原始方式）
```lua
-- 路径配置
M.snippetPath = vim.fn.stdpath('config') .. '/oiSnippets/'  -- ~/.config/nvim/oiSnippets/
M.codePath = vim.fn.stdpath('config') .. '/code/'           -- ~/.config/nvim/code/
```

#### 方式二：使用 lazy.nvim 管理插件（推荐）
```lua
-- 路径配置（当前实现）
M.snippetPath = vim.fn.stdpath('data') .. '/rbook/oiSnippets/'  -- ~/.local/share/nvim/rbook/oiSnippets/
M.codePath = vim.fn.stdpath('data') .. '/rbook/code/'           -- ~/.local/share/nvim/rbook/code/

-- 内置路径（相对插件目录）
M.builtinCodePath = plugin_root .. '../code/'
M.builtinSnippetPath = plugin_root .. '../oiSnippets/'
```

### 2. 路径优先级

```
1. 用户自定义路径 (setup 函数中配置)
2. 默认用户数据路径 (stdpath('data')/rbook/)
3. 插件内置路径 (相对插件目录)
```

### 3. 路径检测和回退机制

```lua
-- 插件启动时的路径处理
local function get_effective_paths()
  local code_paths = {
    user_path,        -- 用户自定义
    M.codePath,       -- 默认用户路径
    M.builtinCodePath -- 内置路径
  }
  
  for _, path in ipairs(code_paths) do
    if vim.fn.isdirectory(path) then
      return path
    end
  end
  
  -- 都不存在则创建默认路径
  vim.fn.mkdir(M.codePath, "p")
  return M.codePath
end
```

## ⚙️ lazy.nvim 配置选项

### 基础配置
```lua
{
  "your-username/rbook.nvim",
  dependencies = { "folke/snacks.nvim" },
  config = function()
    require("rbook").setup({
      -- 完全自定义路径
      snippetPath = "/path/to/my/snippets/",
      codePath = "/path/to/my/code/",
      
      -- 使用内置代码片段 + 用户片段
      useBuiltinSnippets = true,
      
      -- 禁用内置代码片段，只使用用户片段
      useBuiltinSnippets = false,
    })
  end
}
```

### 开发模式配置
```lua
{
  dir = "~/path/to/rbook_nunjucks",  -- 本地开发路径
  dependencies = { "folke/snacks.nvim" },
  config = function()
    -- 开发配置
    require("rbook").setup({
      dev = true,  -- 开发模式
      verbose = true, -- 详细日志
    })
  end,
}
```

## 🔧 路径相关的改进建议

### 1. rbook_code.lua 改进
```lua
-- 当前问题：硬编码路径
M.codeRoot = vim.fn.stdpath('config') .. '/code/'

-- 改进方案：支持多路径
M.codeRoots = {
  -- 用户自定义路径（最高优先级）
  user_path = nil,  -- 从 rbook.lua setup 传入
  
  -- 默认用户数据路径
  data_path = vim.fn.stdpath('data') .. '/rbook/code/',
  
  -- 插件内置路径
  builtin_path = plugin_root .. '../code/',
}

function M.set_paths(opts)
  M.codeRoots.user_path = opts.codePath
  M.snippetRoots.user_path = opts.snippetPath
end
```

### 2. 路径搜索函数
```lua
function M.get_available_code_roots()
  local available_roots = {}
  
  for name, path in pairs(M.codeRoots) do
    if path and vim.fn.isdirectory(path) then
      table.insert(available_roots, {
        name = name,
        path = path,
        priority = name == "user_path" and 1 or 
                   name == "data_path" and 2 or 3
      })
    end
  end
  
  -- 按优先级排序
  table.sort(available_roots, function(a, b) 
    return a.priority < b.priority 
  end)
  
  return available_roots
end
```

### 3. 合并多源代码片段
```lua
function M.get_all_snippets_from_all_roots()
  local all_snippets = {}
  local roots = M.get_available_code_roots()
  
  for _, root in ipairs(roots) do
    local snippets = M.get_snippets_from_path(root.path)
    for _, snippet in ipairs(snippets) do
      snippet.source = root.name  -- 标记来源
      table.insert(all_snippets, snippet)
    end
  end
  
  return all_snippets
end
```

## 📋 迁移指南

### 从旧版本迁移到 lazy.nvim

1. **备份现有代码片段**
```bash
# 备份配置目录下的片段
cp -r ~/.config/nvim/oiSnippets ~/backup/rbook_oisnippets
cp -r ~/.config/nvim/code ~/backup/rbook_code
```

2. **迁移到数据目录**
```bash
# 迁移到新位置
mkdir -p ~/.local/share/nvim/rbook/
cp -r ~/backup/rbook_oisnippets ~/.local/share/nvim/rbook/oiSnippets
cp -r ~/backup/rbook_code ~/.local/share/nvim/rbook/code
```

3. **更新配置**
```lua
-- 新配置
require("rbook").setup({
  -- 使用新的默认路径，无需额外配置
})

-- 或者保持原有路径
require("rbook").setup({
  snippetPath = vim.fn.stdpath('config') .. '/oiSnippets/',
  codePath = vim.fn.stdpath('config') .. '/code/',
})
```

## 🚀 最佳实践

### 1. 推荐的插件目录结构
```
rbook.nvim/
├── lua/rbook/           -- 命名空间
│   ├── init.lua        -- 主入口
│   ├── core.lua        -- 核心逻辑
│   ├── config.lua      -- 配置管理
│   └── utils.lua       -- 工具函数
└── assets/             -- 资源文件
    ├── snippets/       -- 内置片段
    ├── templates/      -- 模板文件
    └── examples/       -- 示例代码
```

### 2. 路径配置建议
- ✅ 使用 `stdpath('data')` 存储用户数据
- ✅ 插件内置代码片段放在插件目录
- ✅ 支持用户完全自定义路径
- ✅ 提供智能路径检测和回退

### 3. 兼容性考虑
- 向后兼容旧的路径配置
- 提供平滑迁移工具
- 清晰的路径优先级说明

这样的设计让插件既能在开发环境直接使用，也能作为 lazy.nvim 插件分发。