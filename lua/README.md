# rbook 代码片段管理插件

这是一个为 Neovim 设计的代码片段管理插件，支持从多个目录管理和插入代码片段。

## 文件结构

```
lua/
├── README.md              # 说明文件（本文件）
├── rbook.lua             # 主插件文件
└── rbook_code.lua        # 代码片段信息管理模块
```

## 功能特性

### 1. 多源代码片段管理
- **oiSnippets 目录**: 存放常用的代码片段
- **code 目录**: 存放完整的代码示例和模板

### 2. 智能代码片段识别
- 自动根据文件扩展名分类（cpp, py, lua, java等）
- 支持模板文件特殊处理（如日期替换）
- 自动添加折叠标记

### 3. 多种选择方式
- 文件浏览器模式选择
- 关键词搜索
- 按分类浏览

### 4. 便捷的快捷键
- `<leader>os`: 从 oiSnippets 插入代码片段
- `<leader>oc`: 从 code 目录插入代码片段
- `<leader>of`: 搜索代码片段
- `<leader>ob`: 按分类浏览代码片段

## 安装和配置

### 1. 目录设置

插件会在以下目录创建和管理代码片段：
- `~/.config/nvim/oiSnippets/` - 常用代码片段
- `~/.config/nvim/code/` - 完整代码示例

### 2. 在 init.lua 中配置

```lua
-- 加载插件
local rbook = require("rbook")

-- 基础配置（可选）
rbook.setup({
  snippetPath = "~/.config/nvim/oiSnippets/",  -- 自定义片段路径
  codePath = "~/.config/nvim/code/"             -- 自定义代码路径
})
```

## 使用方法

### 命令模式

#### 1. OISnipChoose - 从 oiSnippets 选择插入
```vim
:OISnipChoose
```
- 弹出文件选择器
- 选择文件后直接插入到光标位置

#### 2. OICodeSnip - 从 code 目录选择插入
```vim
:OICodeSnip
```
- 显示代码片段列表，包含文件名、分类和描述
- 支持预览功能
- 选择后自动插入并添加折叠标记

#### 3. OISearchSnip - 搜索代码片段
```vim
:OISearchSnip
```
- 输入关键词进行搜索
- 支持文件名、描述、分类的模糊匹配
- 显示搜索结果并可选择插入

#### 4. OIBrowseSnip - 按分类浏览
```vim
:OIBrowseSnip
```
- 首先显示所有分类
- 选择分类后显示该分类下的所有代码片段

### 快捷键模式

| 快捷键 | 功能 | 说明 |
|--------|------|------|
| `<leader>os` | OISnipChoose | 从 oiSnippets 快速插入 |
| `<leader>oc` | OICodeSnip | 从 code 目录快速插入 |
| `<leader>of` | OISearchSnip | 搜索代码片段 |
| `<leader>ob` | OIBrowseSnip | 按分类浏览 |

## 代码片段特殊处理

### 模板文件处理
- 文件名包含 "template" 的文件会被视为模板
- **simaple_template.cpp** 会自动替换日期字符串
- 不会添加折叠标记

### 普通代码片段
- 自动添加折叠标记：
  ```cpp
  //oisnip_beginfilename.cpp
  // 代码内容
  //oisnip_end
  ```

### 日期替换示例
模板文件中的 `2025-10-02 10:34:43` 会被自动替换为当前日期时间。

## 代码片段组织建议

### oiSnippets 目录结构
```
oiSnippets/
├── cpp/
│   ├── fast_io.cpp
│   ├── debug_template.cpp
│   └── ...
├── python/
│   ├── input_template.py
│   └── ...
└── templates/
    ├── simaple_template.cpp
    └── ...
```

### code 目录结构
```
code/
├── algorithms/
│   ├── dfs/
│   ├── bfs/
│   └── ...
├── data_structures/
│   ├── stack/
│   ├── queue/
│   └── ...
├── problems/
│   ├── dp/
│   ├── graph/
│   └── ...
└── templates/
    ├── contest_template.cpp
    └── ...
```

## 代码分类

插件会自动识别以下文件类型：

| 扩展名 | 分类 | 描述 |
|--------|------|------|
| `.cpp` | cpp | C++ 代码片段 |
| `.py` | python | Python 代码片段 |
| `.lua` | lua | Lua 代码片段 |
| `.java` | java | Java 代码片段 |
| 包含 `template` | template | 代码模板 |

## 使用场景示例

### 1. 算法竞赛快速编码
1. 使用 `<leader>os` 插入竞赛模板
2. 使用 `<leader>of>` 搜索特定算法实现
3. 代码自动折叠，便于导航

### 2. 学习代码管理
1. 按分类浏览 (`<leader>ob`) 查看特定主题的代码
2. 搜索功能快速找到相关实现
3. 预览功能查看代码内容

### 3. 团队代码分享
- 在 code 目录存放团队常用的代码模板
- 使用搜索功能快速找到团队成员的最佳实践

## 高级功能

### 自定义快捷键
可以在 setup 中覆盖默认快捷键：

```lua
rbook.setup({
  -- 自定义快捷键
})

vim.keymap.set('n', '<F2>', ":OICodeSnip<CR>", { desc = "Custom snippet insert" })
```

### 批量管理代码片段
可以通过脚本批量添加代码片段：

```bash
# 批量复制代码文件到 code 目录
cp ~/algorithms/* ~/.config/nvim/code/algorithms/
```

## 故障排除

### 1. 路径不存在
插件会自动创建所需目录，如果遇到权限问题，请手动创建：
```bash
mkdir -p ~/.config/nvim/oiSnippets
mkdir -p ~/.config/nvim/code
```

### 2. Snacks 依赖
确保安装了 snacks.nvim 插件：
```lua
require("snacks")
```

### 3. 快捷键冲突
如果快捷键冲突，可以在 setup 中自定义或禁用默认快捷键。

## 版本信息

- 当前版本: 1.0.0
- 兼容 Neovim 0.7+
- 需要 snacks.nvim 插件