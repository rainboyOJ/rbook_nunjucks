# Rbook HTTP API 使用指南

Rbook HTTP API 提供算法电子书目录、文章内容、代码模板和标签统计。目录、筛选和标签接口使用启动时的搜索索引；文章详情会按索引定位后重新读取当前 Markdown。

HTML 文档：[当前页面](/api)；Markdown 原文：[/api/help?format=md](/api/help?format=md)。

## 快速开始

先设置服务地址：

```bash
export BASE_URL="${RBOOK_BASE_URL:-http://127.0.0.1:3000}"
```

HTML 文档会把 `$BASE_URL` 替换为当前请求的协议和主机；Markdown 原文保留 `$BASE_URL`，便于直接运行示例。

启动开发服务器：

```bash
npm run dev
```

检查服务和索引：

```bash
curl "$BASE_URL/api/health"
```

## 公共约定

### 响应格式

除 `/api` 和 `/api/help` 文档接口外，公共接口返回 JSON。时间字段 `generatedAt` 使用 ISO 8601 格式。

响应中的 `url` 是以 `/` 开头的站内根相对路径，不包含部署域名。客户端需要时可用 `BASE_URL + url` 拼成完整地址。`path` 是相对于 `book/pages` 或 `book/code` 的内容路径，不会返回服务器的本机绝对路径。

文章的 `description` 只读取 front matter 中显式填写的 `description`，并兼容旧字段 `desc`。缺失时返回空字符串，不会使用正文摘要自动填充；需要描述的重要文章应在 Markdown 中单独维护该元数据。

### 缓存策略

`/api`、`/api/*` 的成功和错误响应统一包含：

```http
Cache-Control: no-store
```

这样开发模式下修改文章并重新请求时不会读到浏览器缓存。如果生产环境以后启用缓存，需要同时修改服务端策略、本文档和 API 合约测试。

### 标签筛选

`/api/pages` 和 `/api/codes` 接受 `tag` 参数。多个标签用逗号分隔，采用 OR 语义，只要条目包含任意一个标签就会匹配。

```bash
curl -G --data-urlencode "tag=树上算法,图论" "$BASE_URL/api/pages"
```

### 分页

列表接口 `/api/pages` 和 `/api/codes` 支持：

- `limit`：本页数量。`/api/pages` 默认 50，`/api/codes` 未指定时返回全部模板；显式指定时最大 50，非法值回退到默认值。
- `offset`：从零开始的偏移量，默认 0；负数或非法值回退到 0。

列表响应中的 `total` 是筛选后的总数，`items` 是当前页的数据。

## 接口总览

| 方法 | 路径 | 用途 |
| --- | --- | --- |
| `GET` | `/api` | 将本文档渲染成 HTML |
| `GET` | `/api/help` | API HTML 文档或 Markdown 原文 |
| `GET` | `/api/health` | 服务状态和索引统计 |
| `GET` | `/api/site` | 站点元信息 |
| `GET` | `/api/catalog` | 文章目录；默认仅返回可见文章 |
| `GET` | `/api/pages` | 文章详情、筛选和分页 |
| `GET` | `/api/codes` | 代码模板详情、筛选和分页 |
| `GET` | `/api/tags` | 文章与代码标签统计 |
| `GET` | `/api/diagnostics` | 开发服务器诊断信息（仅 dev） |
| `POST` | `/api/admin/reindex` | 重建搜索索引 |

## 文档

### `GET /api`

返回由本 Markdown 文件渲染的 HTML。示例中的 `$BASE_URL` 会使用请求的 `Host` 和协议替换。

```bash
curl "$BASE_URL/api"
```

### `GET /api/help`

不带参数时返回与 `/api` 相同内容的 HTML：

```bash
curl "$BASE_URL/api/help"
```

`format=md` 返回本文件未经替换的原始内容，响应类型为 `text/markdown`：

```bash
curl "$BASE_URL/api/help?format=md"
```

## 健康状态

### `GET /api/health`

```bash
curl "$BASE_URL/api/health"
```

响应结构：

```json
{
  "ok": true,
  "generatedAt": "2026-07-25T11:21:13.109Z",
  "stats": {
    "pages": 432,
    "codes": 189,
    "errors": 0
  }
}
```

## 站点信息

### `GET /api/site`

```bash
curl "$BASE_URL/api/site"
```

响应包含 `site`、`stats` 和 `generatedAt`：

```json
{
  "site": {
    "title": "我的算法书",
    "author": "rainboy",
    "description": "这是一本关于算法的书",
    "github_repository": "https://github.com/rainboyOJ/rbook_nunjucks"
  },
  "stats": {
    "pages": 432,
    "codes": 189,
    "errors": 0
  },
  "generatedAt": "2026-07-25T11:21:13.109Z"
}
```

## 文章目录

### `GET /api/catalog`

默认只返回 `visible !== false` 的目录文章。传入 `includeHidden=true` 时返回全部已索引文章，包括未列入首页目录的文章：

```bash
curl "$BASE_URL/api/catalog?includeHidden=true"
```

每个条目都包含 `prerequisites` 和 `visible`。`prerequisites` 是直接前置文章的 ID 数组；文章 B 声明 `prerequisites: [article-a]` 表示关系 `article-a -> B`。默认响应还包含 `headings`、`navTrail`、`codeTemplates` 和 `source`。

使用 `compact=true` 只返回适合快速浏览的字段：

```bash
curl "$BASE_URL/api/catalog?compact=true"
```

```json
{
  "generatedAt": "2026-07-25T11:21:13.109Z",
  "total": 123,
  "items": [
    {
      "id": "dsu-on-tree",
      "title": "树上启发式合并",
      "description": "...",
      "tags": ["树上算法", "启发式合并", "DSU on tree"],
      "prerequisites": [],
      "path": "algorithm/dsu_on_tree/index.md",
      "url": "/algorithm/dsu_on_tree/index.html",
      "visible": true
    }
  ]
}
```

## 文章

### `GET /api/pages?id=<id>`

按稳定 ID 读取一篇文章：

```bash
curl -G --data-urlencode "id=dsu-on-tree" "$BASE_URL/api/pages"
```

响应包含 `id`、`title`、`path`、`url`、`description`、`tags`、`categories`、`frontMatter`、`headings`、`markdown` 和 `navTrail`。`markdown` 是可独立阅读的完整文档：保留文章 front matter，展开 `@include_md`，并把 `@include-code` 替换成带语言标记的 fenced code block。代码引用支持相对于文章的路径、以 `/code/` 开头的内容路径，以及 `book/code.yaml` 中的模板 ID。接口不返回可由 Markdown 派生的 `html`、`text`、`excerpt`，也不暴露内部索引字段 `visible`、`source`；处理详情请求时不会执行 HTML 渲染。

### `GET /api/pages`

不传 `id` 时返回文章列表，可使用标签和分页参数：

```bash
curl -G \
  --data-urlencode "tag=树上算法" \
  --data-urlencode "limit=20" \
  --data-urlencode "offset=0" \
  "$BASE_URL/api/pages"
```

每个列表项包含 `id`、`title`、`path`、`url`、`description`、`tags`、`visible`、`source`、`navTrail` 和 `codeTemplates`。

## 代码模板

### `GET /api/codes?id=<id>`

按稳定 ID 读取模板元数据和引用该模板的文章：

```bash
curl -G --data-urlencode "id=dsu-on-tree-color-count" "$BASE_URL/api/codes"
```

响应字段包括 `id`、`path`、`url`、`description`、`language`、`tags`、`complexity`、`author`、`aliases` 和 `articles`。`articles` 中每项包含文章的 `id`、`title`、`path` 和根相对 `url`。

传入 `includeContent=true` 可同时读取模板源码：

```bash
curl -G \
  --data-urlencode "id=dsu-on-tree-color-count" \
  --data-urlencode "includeContent=true" \
  "$BASE_URL/api/codes"
```

响应会额外包含字符串字段 `content`。

### `GET /api/codes`

不传 `id` 时返回模板列表，支持 `tag`、`limit`、`offset` 和 `includeContent`。未指定 `limit` 时返回全部模板；如果需要分页，再显式传入 `limit`：

```bash
curl -G \
  --data-urlencode "tag=树形数据结构" \
  --data-urlencode "limit=20" \
  --data-urlencode "offset=0" \
  "$BASE_URL/api/codes"
```

## 标签统计

### `GET /api/tags`

```bash
curl "$BASE_URL/api/tags"
```

响应中的 `articleTags` 和 `codeTags` 均为 `{ tag, count }` 数组，按数量降序排列：

```json
{
  "generatedAt": "2026-07-25T11:21:13.109Z",
  "articleTags": [{ "tag": "图论", "count": 20 }],
  "codeTags": [{ "tag": "图", "count": 12 }]
}
```

## 开发诊断

### `GET /api/diagnostics`

该接口只在 `npm run dev` 的开发服务器中注册。生产服务不会暴露它，访问生产地址会返回 `API_ROUTE_NOT_FOUND`。

```bash
curl "$BASE_URL/api/diagnostics"
```

响应只包含相对路径、级别、阶段和消息，不暴露服务器绝对路径或源码：

```json
{
  "mode": "development",
  "generatedAt": "2026-07-27T00:00:00.000Z",
  "stats": {
    "pages": 432,
    "codes": 189,
    "errors": 0,
    "warnings": 1
  },
  "issues": [
    {
      "level": "WARNING",
      "filePath": "book/pages/example.md",
      "message": "missing tags",
      "stage": "startup"
    }
  ]
}
```

启动时的 fatal pre-check error 仍然会阻止开发服务器启动；诊断页面用于查看启动 warning，以及服务器运行后访问文章触发的单页校验和渲染问题。

## 管理接口

### `POST /api/admin/reindex`

重建搜索索引。设置了 `RBOOK_ADMIN_TOKEN` 时，需要通过 Bearer token 或 `x-rbook-token` 请求头认证。

```bash
curl -X POST \
  -H "Authorization: Bearer $RBOOK_ADMIN_TOKEN" \
  "$BASE_URL/api/admin/reindex"
```

成功响应包含 `ok`、`generatedAt`、`stats` 和 `errors`。

## 错误响应

按 ID 查询不存在的资源会返回 HTTP 404 和稳定错误码：

```json
{
  "error": "PAGE_NOT_FOUND",
  "message": "page with id 'missing-page' not found"
}
```

```json
{
  "error": "CODE_NOT_FOUND",
  "message": "code with id 'missing-code' not found"
}
```

模板元数据存在但源文件缺失时返回 `CODE_FILE_NOT_FOUND`。未注册的 `/api/*` 路由返回：

```json
{
  "error": "API_ROUTE_NOT_FOUND",
  "message": "api route not found"
}
```

所有上述错误响应同样使用 `Cache-Control: no-store`。

## Python 客户端

仓库提供统一客户端 `scripts/rbook.py`：

```bash
python3 scripts/rbook.py health
python3 scripts/rbook.py site
python3 scripts/rbook.py catalog
python3 scripts/rbook.py find "树上 启发式" --limit 10
python3 scripts/rbook.py pages --id dsu-on-tree
python3 scripts/rbook.py codes --id dsu-on-tree-color-count
python3 scripts/rbook.py tags
```

列表命令默认输出 TSV。文章列表列为 `#`、`id`、`title`、`description`、`tags`；代码列表列为 `#`、`id`、`title`、`language`、`tags`；标签列表列为 `#`、`type`、`tag`、`count`。`#` 是当前输出的行号，每次从 1 开始。字段中的制表符、换行和连续空白会压缩成一个空格，确保每条记录只占一行；多个标签使用英文逗号连接。

目录、搜索结果、文章列表、代码列表和标签列表也支持 `--table`，以 ASCII 表格输出相同字段，便于人类在终端中阅读。`--table` 与 `--json` 互斥；文章或代码使用 `--id` 请求详情时不能使用 `--table`：

```bash
python3 scripts/rbook.py codes --table
python3 scripts/rbook.py pages --table --limit 10
```

`find` 只在文章 ID、标题、描述和标签中查找，多个关键词使用 AND 语义；它不切分或搜索文章正文。`pages` 和 `codes` 的列表模式支持 `--tag`、`--limit`、`--offset`。

详情模式默认直接返回可用原文。文章 Markdown 包含 front matter，且 `@include-code` 已替换为对应源码：

```bash
# 完整 Markdown
python3 scripts/rbook.py pages --id dsu-on-tree

# 完整代码模板源码
python3 scripts/rbook.py codes --id dsu-on-tree-color-count
```

在任意子命令末尾添加 `--json` 可切换为 JSON：

```bash
python3 scripts/rbook.py catalog --json
python3 scripts/rbook.py find "树上 启发式" --limit 10 --json
python3 scripts/rbook.py pages --id dsu-on-tree --json
python3 scripts/rbook.py codes --id dsu-on-tree-color-count --json
```

成功的 JSON 响应使用两个空格缩进，便于在终端中直接阅读。写入标准错误的 JSON 错误保持单行，方便日志工具处理。

列表 JSON 只包含 `total` 和精简后的 `items`，不包含行号。文章与代码列表项都包含 `tags` 字符串数组。文章详情 JSON 与 `/api/pages?id=<id>` 使用相同的 11 个精简字段。代码详情 JSON 包含完整元数据与源码；`health`、`site`、`tags` 的 JSON 保留对应 API 的完整响应。

`--id` 不能和 `--tag`、`--limit`、`--offset`、`--table` 同时使用。错误默认以 `ERROR_CODE: message` 写入标准错误；JSON 模式则输出 `{ "error": "...", "message": "..." }`，并以非零状态码退出。

可用 `RBOOK_BASE_URL` 切换服务地址。
