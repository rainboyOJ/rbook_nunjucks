# rbook monorepo structure

This repository uses npm workspaces for reusable runtime code. Book content is kept under `book/`, and the current site shell is kept under `site/`.

## Packages

```text
packages/rbook-core      static build, config loading, shared paths, template rendering
packages/rbook-markdown  Markdown parser and markdown-it plugins
packages/rbook-search    page collection, chunking, Fuse search index
packages/rbook-server    Fastify static/API server
packages/rbook-cli       CLI entrypoint
```

The root `bin/rbook.js` and legacy `src/*` entrypoints are compatibility shims that forward to the workspace packages.

## Content And App

The current algorithm book content lives in `book`:

```text
book/pages/
book/book.yaml
book/code/
```

The app shell for that content lives in `site`:

```text
site/theme/
site/public/
site/markdown-style/
site/widgets/
```

`site/widgets/` 下的工具页面都使用单文件 HTML，不再使用 Vue、Vite 或独立的 TypeScript 构建项目。当前 widget 包括：

- `code_template_filter/index.html` → `/code_template`
- `explore/index.html` → `/explore`
- `article_inspector/index.html` → `/article_inspector`
- `tags/index.html` → `/tags`
- `relations/index.html` → `/relations`
- `practice/index.html` → `/practice`
- `diagnostics/index.html` → `/diagnostics`（仅开发服务器）

页面通过同源公共 API 读取数据，运行时构建会按显式映射直接复制到对应的 `dist/` 目录。代码模板页按需请求模板源码；其他页面只在需要时请求文章详情或诊断数据。

The shared path module is `@rbook/core/paths`. It defaults to `site` for app assets and `book` for book content. Override them with `RBOOK_APP_DIR` and `RBOOK_CONTENT_DIR`.

## Common Commands

```bash
npm run build
npm run build:index
npm run build:all
npm run serve
```
