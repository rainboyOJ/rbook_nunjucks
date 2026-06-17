---
name: rbook-http
description: Use the local rbook dynamic ebook HTTP API to search and read algorithm book content. Use when answering questions from this rbook site, locating pages, or gathering context from the ebook through HTTP.
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

## OJ 解题工作流

当用户希望根据本项目的文章和模板代码完成 OJ 题目、生成题解、保持代码风格时，优先使用 AI 专用接口：

1. 调用 `GET /api/ai/catalog` 获取正式文章目录、文章引用链接和 `code_template` 元数据。
2. 根据题目关键词、算法模型、模板标签选择相关文章。
3. 调用 `GET /api/ai/page-context?path=<article>&includeCode=true` 获取完整文章、引用信息、模板代码和正文 `@include-code` 代码。
4. 如果只需要某份模板代码，调用 `GET /api/ai/code?path=/code/...`。
5. 写 C++ 代码时优先模仿 `codeTemplates[].content` 的结构、变量命名、注释密度和竞赛风格。
6. 写题解时说明使用了哪些本项目文章，并使用 `citation.href` 生成可点击引用。
7. 不使用本 API 查询题目数据；题目服务已经拆分到独立项目。

题解引用格式示例：

```md
本文使用了本项目中的 [点双连通分量](https://rbook2.roj.ac.cn/graph/bcc/index.html) 文章内容。
```

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

### AI Catalog

```bash
GET /api/ai/catalog
GET /api/ai/catalog?scope=visible
GET /api/ai/catalog?scope=all
```

Returns an AI-friendly article catalog. Default `scope=visible` only returns pages shown in the homepage table of contents. Use `scope=all` only when internal or hidden pages are needed.

Each article includes:

- `path`, `url`, `href`
- `title`, `description`, `excerpt`
- `tags`, `categories`, `headings`
- `navTrail`, `visible`, `source`
- `codeTemplates`
- `citation`

Use this endpoint first when selecting which rbook article or code template should guide an OJ solution.

### AI Page Context

```bash
GET /api/ai/page-context?path=graph/bcc/index.md
GET /api/ai/page-context?path=graph/bcc/index.md&includeCode=true
GET /api/ai/page-context?path=graph/bcc/index.md&includeCode=true&includeHtml=true
```

Returns one full article context for agent reasoning:

- `article.markdown`
- `article.text`
- `article.headings`
- `article.frontMatter`
- `article.citation`
- `codeTemplates`
- `includedCode`

By default it does not include rendered HTML. Add `includeHtml=true` only when rendered markup is required. Add `includeCode=true` when the agent needs template source code.

`codeTemplates` comes from front matter `code_template` and should be preferred when generating OJ code. `includedCode` comes from article body `@include-code(...)` blocks and may include explanatory snippets.

### AI Code

```bash
GET /api/ai/code?path=/code/graph/v-bcc.cpp
```

Returns a single code template:

- `path`
- `url`
- `href`
- `language`
- `content`

This endpoint only reads files under the ebook `book/code/` directory. Do not use arbitrary filesystem paths.

## Answering Guidance

When answering ebook questions:

- cite page titles and paths from API responses when useful;
- for OJ solutions, cite `citation.href` from `/api/ai/catalog` or `/api/ai/page-context`;
- prefer `/api/ai/page-context?includeCode=true` when code style matters;
- use chunk search before full page fetch;
- fetch full pages for detailed algorithm explanations;
- do not use this rbook API for problem lookup; problem data lives in a separate service/project;
- do not invent content that is not present in the API results.
