# Rbook HTTP API 使用指南

本文档介绍如何使用 rbook HTTP API 查询算法电子书内容。

在线查看：[HTML 文档](/api) | [原始 Markdown](/api/md)

## 快速开始

### 1. 设置服务地址

```bash
export BASE_URL="${RBOOK_BASE_URL:-http://127.0.0.1:3000}"
```

HTML 文档会把 `$BASE_URL` 自动替换为当前请求的服务地址；`/api/md` 返回的原始 Markdown 保留变量写法。

### 2. 启动服务器

```bash
# 开发模式（文章按 URL 实时渲染）
npm run dev

# 生产模式
npm run build:all
npm run serve
```

默认地址：`$BASE_URL`

### 3. 查看 API 文档

浏览器访问：`$BASE_URL/api`

### 4. 健康检查

```bash
curl "$BASE_URL/api/health"
```

返回：
```json
{
  "ok": true,
  "generatedAt": "2026-07-25T11:21:13.109Z",
  "stats": {
    "pages": 432,
    "chunks": 4384,
    "errors": 0
  }
}
```

---

## API 分类

### Public API (`/api/*`)
通用查询接口，适合前端和通用客户端使用。

### AI API (`/api/ai/*`)
为 AI Agent 优化的接口，返回紧凑格式，只包含相对路径。

### Admin API (`/api/admin/*`)
管理接口，需要认证（需设置 `RBOOK_ADMIN_TOKEN`）。

---

## 接口总览

| 方法 | 路径 | 用途 |
|---|---|---|
| `GET` | `/api` | 返回由本文档渲染的 HTML |
| `GET` | `/api/md` | 返回本文档的原始 Markdown |
| `GET` | `/api/health` | 服务健康状态和索引统计 |
| `GET` | `/api/site` | 站点元信息 |
| `GET` | `/api/toc` | 电子书目录树 |
| `GET` | `/api/nav` | 与 `/api/toc` 相同的导航树 |
| `GET` | `/api/pages` | 页面元数据列表 |
| `GET` | `/api/page` | 单篇文章的完整数据 |
| `GET` | `/api/search` | 页面级全文搜索 |
| `GET` | `/api/chunks/search` | 内容块级搜索 |
| `GET` | `/api/ai/catalog` | AI 友好的文章和代码模板目录 |
| `GET` | `/api/ai/page-context` | AI 解题所需的文章上下文 |
| `GET` | `/api/ai/code` | 读取 `book/code` 下的模板代码 |
| `POST` | `/api/admin/reindex` | 重建搜索索引，需要管理员认证 |

---

## 常用接口

### 1. 站点信息

**获取站点元信息**

```bash
curl "$BASE_URL/api/site"
```

返回站点配置、统计信息和索引生成时间。

**获取目录树**

```bash
curl "$BASE_URL/api/toc"
```

返回完整的章节目录结构。

---

### 2. 页面查询

**列出所有页面**

```bash
# 所有页面
curl "$BASE_URL/api/pages"

# 只显示可见页面
curl "$BASE_URL/api/pages?visible=true"
```

**获取单个页面详情**

```bash
curl -G --data-urlencode "path=graph/bcc/index.md" \
  "$BASE_URL/api/page"
```

返回完整页面内容：
- `frontMatter` - 元数据
- `markdown` - Markdown 源码
- `html` - 渲染后的 HTML
- `text` - 纯文本（用于搜索）
- `headings` - 标题列表
- `chunks` - 文章分块

---

### 3. 搜索

**页面级搜索**

适合先定位相关页面：

```bash
curl -G --data-urlencode "q=二分图" \
  --data-urlencode "limit=5" \
  "$BASE_URL/api/search"
```

**片段级搜索（推荐）**

更精确，返回匹配的段落：

```bash
curl -G --data-urlencode "q=数位DP 状态 记忆化" \
  --data-urlencode "limit=8" \
  --data-urlencode "textLength=900" \
  "$BASE_URL/api/chunks/search"
```

参数说明：
- `q` - 必填，搜索关键词
- `limit` - 可选，返回数量（最大 50，默认 10）
- `textLength` - 可选，每个片段的文本长度（默认 900）
- `includeText` - 可选，设为 `false` 不返回文本内容

---

### 4. AI 专用接口

#### 4.1 获取文章目录

```bash
# 只获取可见文章（默认）
curl "$BASE_URL/api/ai/catalog"

# 获取所有文章（包括隐藏页面）
curl "$BASE_URL/api/ai/catalog?scope=all"
```

返回结构：
```json
{
  "scope": "visible",
  "total": 123,
  "generatedAt": "2026-07-25T11:21:13.109Z",
  "articles": [
    {
      "path": "graph/bcc/index.md",
      "url": "/graph/bcc/index.html",
      "title": "点双连通分量",
      "description": "...",
      "tags": ["图论", "连通性"],
      "categories": ["graph"],
      "codeTemplates": [
        {
          "title": "点双连通分量模板",
          "code": "/code/graph/v-bcc.cpp",
          "codeUrl": "/code/graph/v-bcc.cpp",
          "language": "cpp"
        }
      ],
      "citation": {
        "title": "点双连通分量",
        "path": "graph/bcc/index.md",
        "url": "/graph/bcc/index.html"
      }
    }
  ]
}
```

**重要**：`url` 和 `codeUrl` 都是相对路径，需要自己拼接：
```javascript
const fullUrl = BASE_URL + article.url;
const codeFullUrl = BASE_URL + template.codeUrl;
```

#### 4.2 获取页面上下文

```bash
# 基本用法
curl -G --data-urlencode "path=graph/bcc/index.md" \
  "$BASE_URL/api/ai/page-context"

# 包含代码模板正文
curl -G --data-urlencode "path=graph/bcc/index.md" \
  --data-urlencode "includeCode=true" \
  "$BASE_URL/api/ai/page-context"

# 同时包含 HTML
curl -G --data-urlencode "path=graph/bcc/index.md" \
  --data-urlencode "includeCode=true" \
  --data-urlencode "includeHtml=true" \
  "$BASE_URL/api/ai/page-context"
```

返回结构：
```json
{
  "generatedAt": "2026-07-25T11:21:13.109Z",
  "article": {
    "path": "graph/bcc/index.md",
    "url": "/graph/bcc/index.html",
    "title": "点双连通分量",
    "markdown": "# 点双连通分量\n\n...",
    "text": "点双连通分量 ...",
    "html": "<h1>点双连通分量</h1>...",
    "frontMatter": { ... },
    "headings": [ ... ],
    "citation": { ... }
  },
  "codeTemplates": [
    {
      "source": "frontMatter",
      "title": "点双连通分量模板",
      "code": "/code/graph/v-bcc.cpp",
      "codeUrl": "/code/graph/v-bcc.cpp",
      "language": "cpp",
      "content": "#include <bits/stdc++.h>\n..."
    }
  ],
  "includedCode": [
    {
      "source": "include-code",
      "path": "bcc-example.cpp",
      "code": "bcc-example.cpp",
      "codeUrl": null,
      "language": "cpp",
      "content": "// 示例代码\n..."
    }
  ]
}
```

**字段说明**：
- `codeTemplates` - 来自 frontMatter 的 `code_template`，通常是 `/code/...` 下的模板代码
- `includedCode` - 来自正文中的 `@include-code(...)`，可能是相对路径
- `includeCode=true` 时才返回 `content` 字段

#### 4.3 读取代码模板

```bash
curl -G --data-urlencode "path=/code/graph/v-bcc.cpp" \
  "$BASE_URL/api/ai/code"
```

返回：
```json
{
  "path": "/code/graph/v-bcc.cpp",
  "url": "/code/graph/v-bcc.cpp",
  "language": "cpp",
  "content": "#include <bits/stdc++.h>\n..."
}
```

**安全限制**：
- 只接受 `/code/...` 或 `code/...` 路径
- 只能读取 `book/code/` 目录下的文件
- 不允许 `../` 目录遍历

---

## 使用 Python 脚本

项目提供了 Python 客户端脚本（需要先安装 rbook-http skill）：

### 健康检查

```bash
python .agents/skills/rbook-http/scripts/rbook_health.py --pretty
```

### 获取目录

```bash
# 只获取可见文章
python .agents/skills/rbook-http/scripts/rbook_catalog.py \
  --scope visible --compact --pretty

# 限制返回数量
python .agents/skills/rbook-http/scripts/rbook_catalog.py \
  --limit 10 --pretty
```

### 搜索

```bash
python .agents/skills/rbook-http/scripts/rbook_search.py \
  "数位DP 状态 记忆化" --limit 8 --pretty
```

### 读取页面上下文

```bash
# 包含代码模板
python .agents/skills/rbook-http/scripts/rbook_page_context.py \
  graph/bcc/index.md --include-code --compact --pretty

# 不包含代码（更快）
python .agents/skills/rbook-http/scripts/rbook_page_context.py \
  graph/bcc/index.md --compact --pretty
```

### 读取代码模板

```bash
# 只输出代码内容
python .agents/skills/rbook-http/scripts/rbook_code.py \
  /code/graph/v-bcc.cpp --content-only

# 输出完整 JSON
python .agents/skills/rbook-http/scripts/rbook_code.py \
  /code/graph/v-bcc.cpp --pretty
```

---

## 推荐工作流

### 场景 1：AI 解题

1. **搜索相关文章**
   ```bash
   curl -G --data-urlencode "q=最短路径 Dijkstra" \
     "$BASE_URL/api/chunks/search"
   ```

2. **获取文章上下文和代码模板**
   ```bash
   curl -G --data-urlencode "path=graph/shortest_path/dijkstra/index.md" \
     --data-urlencode "includeCode=true" \
     "$BASE_URL/api/ai/page-context"
   ```

3. **生成题解时引用文章**
   ```markdown
   本题使用了 [Dijkstra 算法]($BASE_URL/graph/shortest_path/dijkstra/index.html)。
   ```

### 场景 2：浏览电子书

1. **获取目录**
   ```bash
   curl "$BASE_URL/api/toc"
   ```

2. **读取页面**
   ```bash
   curl -G --data-urlencode "path=graph/bcc/index.md" \
     "$BASE_URL/api/page"
   ```

### 场景 3：代码模板管理

1. **查看所有模板（通过目录）**
   ```bash
   curl "$BASE_URL/api/ai/catalog" | \
     jq '.articles[].codeTemplates[]'
   ```

2. **读取特定模板**
   ```bash
   curl -G --data-urlencode "path=/code/graph/v-bcc.cpp" \
     "$BASE_URL/api/ai/code"
   ```

---

## 常见问题

### Q1: 如何处理中文路径？

使用 `curl -G --data-urlencode`：

```bash
# ✅ 正确
curl -G --data-urlencode "path=动态规划/背包/index.md" \
  "$BASE_URL/api/page"

# ❌ 错误
curl "$BASE_URL/api/page?path=动态规划/背包/index.md"
```

或者手动 URL 编码：
```bash
curl "$BASE_URL/api/page?path=%E5%8A%A8%E6%80%81%E8%A7%84%E5%88%92/%E8%83%8C%E5%8C%85/index.md"
```

### Q2: AI API 为什么不返回完整 URL？

**设计原因**：
- 服务器可能部署在不同域名
- 客户端知道自己从哪个地址请求
- 由客户端拼接更灵活

**拼接方法**：
```javascript
const BASE_URL = process.env.RBOOK_BASE_URL || 'http://127.0.0.1:3000';
const fullUrl = BASE_URL + article.url;
```

### Q3: 如何区分代码来源？

查看 `source` 字段：
- `source: "frontMatter"` - 来自 frontMatter 的 `code_template`，通常是 `/code/...` 模板
- `source: "include-code"` - 来自正文 `@include-code(...)`，可能是相对路径

### Q4: 搜索没有结果怎么办？

1. 检查索引是否构建：
   ```bash
   curl "$BASE_URL/api/health"
   ```

2. 重建索引：
   ```bash
   npm run build:index
   ```

3. 或者使用 admin API（需要认证）：
   ```bash
   curl -X POST -H "Authorization: Bearer $RBOOK_ADMIN_TOKEN" \
     "$BASE_URL/api/admin/reindex"
   ```

### Q5: 如何切换到线上部署？

修改 `BASE_URL`：
```bash
export RBOOK_BASE_URL=https://rbook2.roj.ac.cn
python .agents/skills/rbook-http/scripts/rbook_health.py --online
```

或者直接传 `--online` 参数（脚本会使用默认线上地址）。

---

## 环境变量

| 变量 | 说明 | 默认值 |
|------|------|--------|
| `HOST` | 服务器监听地址 | `0.0.0.0` |
| `PORT` | 服务器端口 | `3000` |
| `RBOOK_BASE_URL` | API 基础 URL | `http://127.0.0.1:3000` |
| `RBOOK_ADMIN_TOKEN` | Admin API 认证令牌 | 未设置 |
| `RBOOK_APP_DIR` | 站点资源目录 | `site` |
| `RBOOK_CONTENT_DIR` | 书籍内容目录 | `book` |
| `RBOOK_RUNTIME_DIR` | 运行时目录（开发模式） | `/tmp/rbook-dev-{pid}` |

---

## 相关文档

- [API 设计和实现](https://github.com/rainboyOJ/rbook_nunjucks/blob/main/how-to-learn-api.md) - 学习 API 内部实现
- [AI API 字段契约](https://github.com/rainboyOJ/rbook_nunjucks/blob/main/docs/development/ai-api-schema.md) - 字段定义和约定
- [rbook-http skill](https://github.com/rainboyOJ/rbook_nunjucks/blob/main/.agents/skills/rbook-http/SKILL.md) - AI Agent 使用指南
- [项目结构](https://github.com/rainboyOJ/rbook_nunjucks/blob/main/docs/development/monorepo-structure.md) - monorepo 架构说明

---

## 在线 API 文档

启动服务器后访问：`$BASE_URL/api`

文档自动显示当前服务器地址，所有示例可直接复制使用。
