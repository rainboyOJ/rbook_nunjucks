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

`site/widgets/code_template_filter/index.html` 是代码模板页面的唯一源码文件。页面通过同源 `/api/codes` 读取模板列表，按需请求模板源码；它不再使用 Vue、Vite 或独立的 TypeScript 构建项目。运行时构建会直接把该文件复制到 `dist/code_template/index.html`。

The shared path module is `@rbook/core/paths`. It defaults to `site` for app assets and `book` for book content. Override them with `RBOOK_APP_DIR` and `RBOOK_CONTENT_DIR`.

## Common Commands

```bash
npm run build
npm run build:index
npm run build:all
npm run serve
```
