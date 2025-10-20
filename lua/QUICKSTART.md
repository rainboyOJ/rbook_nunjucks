# rbook 代码片段管理插件 - 快速开始指南

## 🚀 快速安装

### 1. 文件放置
将 `rbook.lua` 和 `rbook_code.lua` 放置到您的 Neovim 配置目录：
```bash
# 将文件复制到 lua 目录
cp rbook.lua ~/.config/nvim/lua/
cp rbook_code.lua ~/.config/nvim/lua/
```

### 2. 依赖要求
- Neovim 0.7+
- [snacks.nvim](https://github.com/folke/snacks.nvim) 插件

### 3. 基础配置
在 `init.lua` 中添加：
```lua
-- 确保已安装 snacks.nvim
require("snacks")

-- 加载 rbook 插件
local rbook = require("rbook")
rbook.setup()
```

## 📋 基本使用

### 核心命令
| 命令 | 功能 | 快捷键 |
|------|------|--------|
| `:OISnipChoose` | 从 oiSnippets 选择插入 | `<leader>os` |
| `:OICodeSnip` | 从 code 目录选择插入 | `<leader>oc` |
| `:OISearchSnip` | 搜索代码片段 | `<leader>of` |
| `:OIBrowseSnip` | 按分类浏览 | `<leader>ob` |

### 快速体验

1. **插入模板文件**
   ```
   按 <leader>os → 选择模板文件 → 插入到当前位置
   ```

2. **搜索算法实现**
   ```
   按 <leader>of → 输入 "dfs" → 选择需要的实现
   ```

3. **浏览所有 C++ 代码**
   ```
   按 <leader>ob → 选择 "cpp" 分类 → 浏览代码片段
   ```

## 📁 目录创建

插件会自动创建以下目录结构：
```
~/.config/nvim/
├── oiSnippets/          # 常用代码片段
│   ├── cpp/
│   ├── python/
│   └── templates/
└── code/                # 完整代码示例
    ├── algorithms/
    ├── data_structures/
    └── templates/
```

## 🛠️ 常用场景

### 场景 1: 算法竞赛
```bash
# 1. 插入竞赛模板
<leader>os → 选择 simaple_template.cpp

# 2. 快速搜索图论算法
<leader>of → 输入 "dijkstra" → 选择实现

# 3. 浏览所有动态规划代码
<leader>ob → 选择 "dp" 分类
```

### 场景 2: 日常编程
```bash
# 1. 插入 Python 模板
<leader>os → 选择 python_template.py

# 2. 搜索调试代码
<leader>of → 输入 "debug" → 选择对应语言
```

### 场景 3: 学习整理
```bash
# 1. 按分类查看所有算法
<leader>ob → 选择 "algorithms"

# 2. 查看特定主题
<leader>ob → 选择 "data_structures" → 选择 "tree"
```

## 🎯 使用技巧

### 1. 文件命名建议
```
good_name_template.cpp    # 模板文件
fast_io.cpp              # 功能片段
dijkstra_template.cpp    # 算法模板
debug_macro.h            # 宏定义
```

### 2. 分类自动识别
| 文件特征 | 自动分类 |
|----------|----------|
| 包含 "template" | template |
| `.cpp` 结尾 | cpp |
| `.py` 结尾 | python |
| `.lua` 结尾 | lua |

### 3. 搜索技巧
- 搜索文件名：`dijkstra`
- 搜索描述：`最短路`
- 搜索分类：`cpp`
- 模糊搜索：`dij` 会匹配 `dijkstra`

## 🔧 自定义配置

### 修改快捷键
```lua
vim.keymap.set('n', '<F1>', ":OICodeSnip<CR>", { 
  desc = "我的代码片段" 
})
```

### 添加自定义命令
```lua
vim.api.nvim_create_user_command("MySnip", function()
  vim.cmd("OICodeSnip")
end, { desc = "自定义代码片段" })
```

### 查看统计信息
```lua
-- 添加到配置中
vim.api.nvim_create_user_command("SnipInfo", function()
  local rbook_code = require("rbook_code")
  local snippets = rbook_code.get_all_snippets()
  print("共有 " .. #snippets .. " 个代码片段")
end, {})
```

## 📝 示例代码片段

### C++ 快速输入模板
```cpp
// oiSnippets/cpp/fast_io.cpp
inline int read() {
    int x=0,f=1;char ch=getchar();
    while(ch<'0'||ch>'9'){if(ch=='-')f=-1;ch=getchar();}
    while(ch>='0'&&ch<='9'){x=x*10+ch-'0';ch=getchar();}
    return x*f;
}
```

### Python 模板
```python
# oiSnippets/python/contest_template.py
import sys
input = sys.stdin.readline

def main():
    n = int(input())
    # 你的代码

if __name__ == "__main__":
    main()
```

## ❓ 常见问题

### Q: 如何添加新的代码片段？
A: 直接将文件复制到对应目录即可：
```bash
cp my_algorithm.cpp ~/.config/nvim/code/algorithms/
```

### Q: 快捷键不生效？
A: 检查是否正确调用了 `rbook.setup()`

### Q: 搜索找不到代码？
A: 确保文件放在正确的目录下，重启 Neovim 刷新缓存

### Q: 如何修改代码片段路径？
A: 在 setup 中配置：
```lua
rbook.setup({
  snippetPath = "/path/to/my/snippets/",
  codePath = "/path/to/my/code/"
})
```

## 🆘 获取帮助

1. 查看完整文档：`cat lua/README.md`
2. 查看配置示例：`cat lua/example_config.lua`
3. 检查插件状态：`:lua print(require("rbook"))`
4. 查看错误信息：`:messages`

## 🎉 开始使用

现在您已经准备好使用 rbook 代码片段管理插件了！

1. 重启 Neovim
2. 按 `<leader>oc` 试试插入代码片段
3. 按 `<leader>of>` 试试搜索功能
4. 开始高效编程！

祝您编码愉快！ 🚀