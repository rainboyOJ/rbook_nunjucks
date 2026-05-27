# rbook monorepo structure

This repository uses npm workspaces for reusable runtime code and app-specific book content.

## Packages

```text
packages/rbook-core      static build, config loading, shared paths, template rendering
packages/rbook-markdown  Markdown parser and markdown-it plugins
packages/rbook-search    page collection, chunking, Fuse search index
packages/rbook-server    Fastify static/API server
packages/rbook-cli       CLI entrypoint
```

The root `bin/rbook.js` and legacy `src/*` entrypoints are compatibility shims that forward to the workspace packages.

## App Content

The current algorithm book site lives in `apps/algorithm-book`:

```text
apps/algorithm-book/book/
apps/algorithm-book/book.yaml
apps/algorithm-book/theme/
apps/algorithm-book/public/
apps/algorithm-book/markdown-style/
apps/algorithm-book/third_part/
```

The shared path module is `@rbook/core/paths`. It defaults to `apps/algorithm-book` and can be overridden with `RBOOK_APP_DIR`.

## Common Commands

```bash
npm run build
npm run build:index
npm run build:all
npm run serve
npm --workspace @rbook/algorithm-book run build:all
```
