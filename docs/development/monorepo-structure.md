# rbook monorepo structure

This repository uses npm workspaces for reusable runtime code while keeping the current book site content at the repository root.

## Packages

```text
packages/rbook-core      static build, config loading, shared paths, template rendering
packages/rbook-markdown  Markdown parser and markdown-it plugins
packages/rbook-search    page collection, chunking, Fuse search index
packages/rbook-server    Fastify static/API server
packages/rbook-cli       CLI entrypoint
```

The root `bin/rbook.js` and legacy `src/*` entrypoints are compatibility shims that forward to the workspace packages.

## Site Content

These directories intentionally remain at the repository root for now:

```text
book/
theme/
public/
markdown-style/
third_part/
book.yaml
```

The shared path module is `@rbook/core/paths`. New code should use it instead of deriving paths from local package directories.

## Common Commands

```bash
npm run build
npm run build:index
npm run build:all
npm run serve
node bin/rbook.js build
```
