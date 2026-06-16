# rbook monorepo structure

This repository uses npm workspaces for reusable runtime code. Book content is kept under `content/`.

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

The current algorithm book content lives in `content/algorithm-book`:

```text
content/algorithm-book/book/
content/algorithm-book/book.yaml
content/algorithm-book/code/
```

The app shell for that content lives in `apps/algorithm-book`:

```text
apps/algorithm-book/theme/
apps/algorithm-book/public/
apps/algorithm-book/markdown-style/
apps/algorithm-book/third_part/
```

The shared path module is `@rbook/core/paths`. It defaults to `apps/algorithm-book` for app assets and `content/algorithm-book` for book content. Override them with `RBOOK_APP_DIR` and `RBOOK_CONTENT_DIR`.

## Common Commands

```bash
npm run build
npm run build:index
npm run build:all
npm run serve
npm --workspace @rbook/algorithm-book run build:all
```
