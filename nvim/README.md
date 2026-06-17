# rbook.nvim

`rbook.nvim` 是本算法电子书的 Neovim 代码复用插件。它基于 `snacks.nvim`，从本地仓库读取 `book/code/` 和文章 front matter 里的 `code_template`，让写题时可以快速搜索、预览、插入模板代码。

## 设计目标

- 完全离线可用，不依赖电子书 HTTP API。
- 只服务代码模板复用，不内置 AI 功能。
- 默认不注册快捷键，只提供命令，方便用户用 `lazy.nvim` 管理。
- 代码索引第一次使用时生成，可手动刷新。
- 代码插入默认保持原样，不额外添加折叠注释。

## 依赖

- Neovim 0.10+
- [`folke/snacks.nvim`](https://github.com/folke/snacks.nvim)
- `lyaml`

`lyaml` 用来解析 `book.yaml` 和文章 front matter：

```bash
luarocks install lyaml
```

如果没有安装 `lyaml`，插件不会影响 Neovim 启动；执行命令时会提示安装方式。

## lazy.nvim 安装

把下面配置加入你的 lazy.nvim 插件列表：

```lua
{
  dir = "/home/rainboy/mycode/rbook_nunjucks/nvim",
  name = "rbook.nvim",
  dependencies = {
    "folke/snacks.nvim",
  },
  opts = {
    repo_root = "/home/rainboy/mycode/rbook_nunjucks",
  },
  keys = {
    { "<leader>rc", "<cmd>RbookCode<CR>", desc = "Rbook code templates" },
    { "<leader>rf", "<cmd>RbookCodeFiles<CR>", desc = "Rbook code files" },
    { "<leader>ra", "<cmd>RbookOpenArticle<CR>", desc = "Rbook articles" },
    { "<leader>rd", "<cmd>RbookDoctor<CR>", desc = "Rbook doctor" },
  },
}
```

`repo_root` 建议显式配置为本仓库根目录。未配置时，插件会尝试从 `nvim/` 插件目录反推出仓库根目录。

## 命令

### `:RbookCode`

搜索正式代码模板。

数据来源是文章 front matter 中的 `code_template`。确认后把对应 `book/code/` 文件插入到当前光标行之前。

常用按键：

- `<CR>`：插入纯代码。
- `<C-f>`：带 `// rbook_begin` / `// rbook_end` 标记插入。
- `<C-y>`：复制代码到寄存器。
- `<C-o>`：打开代码文件。
- `<C-a>`：打开关联文章源文件。
- `<C-r>`：刷新索引。

### `:RbookCodeFiles`

浏览 `book/code/` 下的全部代码文件。它是兜底入口，适合查找还没有正式文章绑定的代码。

默认文件类型：

```lua
{ "cpp", "cc", "cxx", "c", "h", "hpp", "py", "sh", "md", "hs" }
```

### `:RbookOpenArticle`

搜索并打开 `book/pages/` 下的文章源文件。

### `:RbookCodeRefresh`

清空并重新生成内存索引。

### `:RbookDoctor`

检查插件关心的一致性问题：

- `repo_root`、`book.yaml`、`book/pages`、`book/code` 是否存在。
- `book.yaml` 是否能被解析。
- 每个 `code_template[].code` 是否指向真实文件。
- 带 `code_template` 的文章是否加入 `book.yaml` 导航或 `glob`。
- 同一份代码是否被多个模板引用。

`RbookDoctor` 不负责检查 Markdown、LaTeX、静态构建或 HTTP API，这些属于电子书构建系统。

## 配置

```lua
{
  dir = "/home/rainboy/mycode/rbook_nunjucks/nvim",
  name = "rbook.nvim",
  dependencies = { "folke/snacks.nvim" },
  opts = {
    repo_root = "/home/rainboy/mycode/rbook_nunjucks",
    insert = {
      fold_markers = false,
    },
    files = {
      extensions = { "cpp", "hpp", "py", "sh", "md", "hs" },
    },
  },
}
```

## 数据约定

正式模板来自文章 front matter：

```yaml
---
title: "最小生成树"
tags: ["图论", "最小生成树", "Kruskal"]
categories: ["图论"]
code_template:
  - title: Kruskal 最小生成树
    desc: "按边权从小到大选边，用并查集避免成环"
    tags: ["图论", "最小生成树", "Kruskal", "并查集"]
    code: /code/graph/最小生成树mst_kruskal.cpp
---
```

`code` 推荐使用 `/code/...` 路径。插件会把它解析到本地 `book/code/...`。
