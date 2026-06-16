# rbook - 极简静态书籍生成器

一个基于 Markdown + Pug 的静态书籍网站生成器，专注于算法和计算机科学内容的展示。

## 特性

- 🚀 **极简设计**：最小化依赖，仅需 markdownit、pug、js-yaml
- 📝 **Markdown 支持**：支持 FrontMatter 和代码高亮
- 🎨 **模板系统**：基于 Pug 的灵活模板引擎
- ⚡ **开发服务器**：内存懒渲染，实时热重载
- 📱 **响应式设计**：移动端友好的界面
- 🔧 **配置简单**：YAML 配置文件，约定优于配置

## 快速开始

### 安装依赖

```bash
sudo apt install -y python3 nodejs npm graphviz
npm install
```

### 构建静态网站

```bash
npm run build
# 或
node bin/rbook.js build
```

### 启动开发服务器

```bash
npm run serve
# 或
node bin/rbook.js serve
```

访问 http://localhost:3000 查看效果

## 项目结构

```
├── book/
│   ├── book.yaml                 # 目录、书站元信息和页面 glob 配置
│   ├── pages/                    # 算法电子书文章 Markdown
│   └── code/                     # 可复用算法模板代码，文章用 /code/... 引用
├── site/
│   ├── theme/                    # Pug 主题模板和主题资源
│   ├── public/                   # favicon、站点 manifest 等静态资源
│   ├── markdown-style/           # Markdown 页面样式
│   ├── widgets/                  # 当前书站运行时使用的交互组件
│   ├── dist/                     # 本地构建输出目录，已忽略
│   └── .search/index.json        # 本地搜索索引，已忽略
├── packages/
│   ├── rbook-core/               # 书籍配置、路径、构建核心
│   ├── rbook-markdown/           # Markdown 渲染插件
│   ├── rbook-search/             # 搜索索引和查询
│   ├── rbook-server/             # Fastify 静态站 + HTTP API
│   └── rbook-cli/                # CLI 入口
├── skills/                       # 给本地 agent 使用的项目 skill
├── docs/                         # 开发和部署文档
└── bin/rbook.js                  # 命令行入口
```

目录边界：

- `book` 是电子书内容根。文章、目录配置和模板代码都在这里。
- `site` 是这本书的网站外壳。它负责主题、样式、公开静态资源和运行时交互组件。
- `packages/*` 是通用 rbook 引擎代码。其他书站也应优先复用这里的能力。
- `book/code` 中的文件在文章里仍然用 `/code/...` 引用，例如 `@include-code(/code/graph/scc.cpp, cpp)`。

## 配置说明

编辑 `book/book.yaml` 文件配置书籍信息：

```yaml
title: 我的书
author: 张三
description: 这是一本关于算法的书

chapters:
  - title: 第一章
    path: chapter1
  - title: 第二章
    path: chapter2
    sections:
      - title: 第一节
        path: chapter2-1
      - title: 第二节  
        path: chapter2-2
```

## 内容编写

### Markdown 文件格式

每个 Markdown 文件可以包含 FrontMatter：

```markdown
---
title: 页面标题
---

# 主标题

这里是内容...

```javascript
// 代码示例
function example() {
  return 'Hello World';
}
```

### 页面类型

- **首页**: `book/pages/index.md` → `/`
- **关于页面**: `book/pages/about.md` → `/about.html`
- **章节页面**: `book/pages/{章节}/index.md` → `/{章节}/`

## 模板系统

### 可用变量

所有模板共享统一的数据结构：

```javascript
{
  site: {
    title: "网站标题",
    author: "作者名",
    description: "网站描述"
  },
  page: {
    title: "页面标题",
    content: "HTML内容",
    type: "index|page|chapter",
    path: "文件路径"
  },
  nav: [
    {title: "首页", path: "/", type: "index"},
    {title: "关于", path: "/about.html", type: "page"},
    {title: "第一章", path: "/chapter1/", type: "chapter"}
  ]
}
```

### 模板文件

- `layout.pug` - 基础布局
- `index.pug` - 首页模板
- `page.pug` - 单页模板
- `chapter.pug` - 章节模板

## 开发特性

### 内存懒渲染

开发服务器采用内存缓存和懒渲染机制：
- 配置和资源在启动时加载到内存
- 页面按需渲染，只在请求时生成
- 文件变化时智能清除缓存
- 零磁盘IO，快速响应

### 文件监听

- **Markdown 变化**：只重新渲染受影响页面
- **主题变化**：重新加载资源并清除所有缓存
- **配置变化**：重新加载配置并清除所有缓存

## 部署

构建后的静态文件在 `site/dist/` 目录，可部署到任何静态托管服务：

```bash
npm run build
# 将 site/dist/ 目录部署到 GitHub Pages、Netlify、Vercel 等
```

## 作为neovim的插件: 代码片段

```lua

```

## 技术栈

- **markdownit**: Markdown 解析
- **pug**: 模板引擎
- **js-yaml**: YAML 配置解析
- **原生 Node.js**: 无额外框架依赖

## 其他方面的使用

- [各种脚本,配置](./dotfiles/readme.md)


## 许可证

本项目采用 [CC BY-NC-SA 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/) 协议。

## 贡献

欢迎提交 Issue 和 Pull Request！

## 更新日志

### v1.0.0
- 初始版本发布
- 支持 Markdown + FrontMatter
- 开发服务器热重载
- 响应式主题设计
