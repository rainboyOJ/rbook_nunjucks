---
name: rbook-http
description: Use the local rbook dynamic ebook HTTP API to search and read algorithm book content. Use when answering questions from this rbook site, locating pages, resolving problem links, or gathering context from the ebook through HTTP.
---

# rbook HTTP Skill

This project exposes a read-only HTTP API for the algorithm ebook. The service is a knowledge source only: do not expect it to call AI models or provide chat endpoints.

## Configuration

Use `RBOOK_BASE_URL` when it is set. Otherwise default to:

```bash
http://127.0.0.1:3000
```

Before using the API, check:

```bash
curl "$RBOOK_BASE_URL/api/health"
```

## Core Workflow

1. Search first unless the exact page path is known.
2. Prefer chunk search for question answering because it returns focused context.
3. Fetch the full page only after selecting the most relevant result.
4. Prefer `markdown` or `text` from `/api/page`; avoid parsing `html` unless the user needs rendered markup.
5. Do not call admin endpoints.

## Endpoints

### Health

```bash
GET /api/health
```

Returns service status, index generation time, and content statistics.

### Site Metadata

```bash
GET /api/site
```

Returns book metadata and statistics.

### Table of Contents

```bash
GET /api/toc
GET /api/nav
```

Returns the chapter tree. Use this when the user asks where a topic lives in the ebook.

### Pages

```bash
GET /api/pages
GET /api/pages?visible=true
```

Returns page metadata. Use `visible=true` when you only need pages shown in the book navigation.

### Page Detail

```bash
GET /api/page?path=dynamic_programming/数位DP/index.md
```

Returns page metadata plus:

- `markdown`: original Markdown content after local includes/front matter handling
- `text`: plain text extracted from Markdown
- `html`: rendered HTML
- `headings`: page headings
- `chunks`: searchable page chunks

For agent reasoning, use `markdown` for structured explanations and code, or `text` for compact context.

### Page Search

```bash
GET /api/search?q=数位DP&limit=5
```

Returns page-level search results with excerpts.

### Chunk Search

```bash
GET /api/chunks/search?q=数位DP 状态 记忆化&limit=8&textLength=900
```

Returns focused chunks. This is the default endpoint for collecting answer context.

### Problem Link Resolution

```bash
GET /api/problems/resolve?oj=luogu&id=P2657
```

Resolves `[[problem: oj,id]]` references to the configured PCS problem URL. The returned URL should be opened in a new browser tab when presented in UI.

## Answering Guidance

When answering ebook questions:

- cite page titles and paths from API responses when useful;
- use chunk search before full page fetch;
- fetch full pages for detailed algorithm explanations;
- keep problem references as links returned by `/api/problems/resolve`;
- do not invent content that is not present in the API results.
