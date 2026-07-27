# Rbook API 优化设计文档

## 整体目标

### API 存在的两个核心目标

1. **AI 编写题解时统一文风**：agent 通过读取 rbook 文章，在编写题目解析时参考内容，并在题解中引用 rbook 文章链接。
   ```markdown
   知识点
       - [kmp rbook2](https://rbook2.roj.ac.cn/string/kmp/index.html)
   ```

2. **AI 编写代码时复用模板**：agent 使用 `book/code/` 下的代码模板，保持代码风格统一，减少用户的理解负担。

### 为什么需要这次优化

1. **现有 API 太复杂**：`/api/ai/*` 和 `/api/*` 两套 API 并存，AI 接口返回字段过于冗余。
2. **缺少统一的文章标识符**：目前靠 `path`（文件路径）定位文章，没有简洁的语义化 ID。
3. **代码模板管理分散**：代码元数据散落在每个文章的 frontMatter 中，没有集中管理。
4. **缺少双向关联**：无法快速知道“某个代码模板被哪些文章引用”。
5. **客户端工具不统一**：现有 Python 脚本是多个独立文件，缺乏统一 CLI。

---

## 设计决策记录

### 决策 1：引入文章唯一 ID

**决策**：每篇文章的 frontMatter 必须包含 `id` 字段，全局唯一。

**格式**：`/^[a-z0-9-]+$/`（小写字母、数字、连字符），例如 `graph-bcc`、`kmp-algo`。

**为什么这样设计**：
- 相比 `path`（如 `graph/bcc/index.md`），`id` 简短、语义化、不易出错
- AI agent 用简洁的 `id` 引用文章更方便
- 使用 `pre_check.ts` 在构建前强制校验，避免遗漏和冲突

**实现要点**：
- `pre_check.ts` 扫描全部 md 文件，检查 `id` 存在且唯一
- 构建工具会将 `id` 提升为搜索索引的顶级字段
- API 使用 `?id=` 参数查询文章（`path` 字段也保留供文件系统使用）

---

### 决策 2：代码模板集中管理（`book/code.yaml`）

**决策**：创建 `book/code.yaml`，作为代码模板元数据的**唯一真实来源**。

**格式**：扁平数组结构

```yaml
codes:
  - id: v-bcc
    path: graph/v-bcc.cpp          # 相对于 book/code/
    description: 点双连通分量 Tarjan 算法
    language: cpp                  # 可选，不填则从扩展名推断
    tags: [图论, 连通性]
    complexity: 时间 O(n+m)，空间 O(n)  # 可选

  - id: dijkstra
    path: graph/dijkstra.cpp
    description: Dijkstra 最短路算法
    language: cpp
    tags: [图论, 最短路]
```

**字段说明**：
- 必填：`id`、`path`、`description`
- 可选：`language`（不填从扩展名推断）、`tags`、`complexity`、`author`、`aliases`
- `path` 相对于 `book/code/` 目录，API 返回时自动拼接 `/code/` 前缀

**为什么这样设计**：
- 集中管理代码元数据，避免在多篇文章中重复维护
- 文章 frontMatter 的 `code_template` 简化为只存代码 `id` 引用数组
- 双向关联：可以从代码 ID 查到所有引用了它的文章

**文章 frontMatter 示例**：
```yaml
---
id: graph-bcc
title: 点双连通分量
code_template: [v-bcc, dijkstra]
tags: [图论, 连通性]
---
```

---

### 决策 3：API 设计简化（RESTful 统一风格）

**决策**：废弃 `/api/ai/*` 路径，统一使用简洁的 RESTful 端点。

**API 端点一览**：

| 端点 | 说明 | 示例 |
|------|------|------|
| `GET /api/health` | 健康检查 | `curl $BASE/api/health` |
| `GET /api/help?format=html\|md` | API 使用文档 | `curl $BASE/api/help?format=md` |
| `GET /api/site` | 站点元信息 | `curl $BASE/api/site` |
| `GET /api/catalog[?compact=true]` | 文章目录 | `curl $BASE/api/catalog?compact=true` |
| `GET /api/pages[?id=&tag=&limit=&offset=]` | 文章列表/详情 | `curl $BASE/api/pages?id=kmp` |
| `GET /api/codes[?id=&tag=&limit=&offset=]` | 代码列表/详情 | `curl $BASE/api/codes?id=v-bcc` |
| `GET /api/tags` | 标签统计 | `curl $BASE/api/tags` |

**查询参数约定**：
- 统一使用 `?id=` 查询单个资源
- 多标签筛选：`?tag=图论,动态规划`（OR 逻辑，精确匹配）
- 分页：`?limit=50&offset=0`
- 粒度控制：`?compact=true`、`?includeContent=true`

**为什么这样设计**：
- RESTful 风格：集合端点复用，通过参数控制粒度，避免端点爆炸
- 简洁统一：废弃了 `/api/ai/*`、`/api/toc`、`/api/nav`、`/api/search`、`/api/chunks/search` 等冗余端点
- `catalog` 足够取代旧的目录和搜索功能

---

### 决策 4：响应格式约定

**单个资源**：
```json
{
  "id": "kmp",
  "title": "KMP 算法",
  "path": "string/kmp/index.md",
  "url": "/string/kmp/index.html",
  "description": "...",
  "tags": ["字符串", "匹配"],
  "frontMatter": { ... },
  "headings": [ ... ],
  "markdown": "...",
  "html": "...",
  "text": "..."
}
```

**列表资源**：
```json
{
  "generatedAt": "2026-07-26T...",
  "total": 432,
  "items": [ ... ]
}
```

**错误格式**：
```json
{
  "error": "PAGE_NOT_FOUND",
  "message": "page with id 'kmp' not found"
}
```

**为什么这样设计**：
- 单个资源直接返回数据，不额外包装
- 列表资源包含 `total` 和 `items`
- 节省 token 消耗（AI agent 调用时更高效）

---

### 决策 5：双向关联（代码 → 文章）

**决策**在构建索引时生成反向映射 `codeToArticles`。

**数据流**：
1. 读取 `book/code.yaml` 获取所有代码元数据
2. 扫描所有文章的 `code_template` 字段
3. 构建 `{ code_id: [article_id, ...] }` 映射
4. 存入搜索索引 `index.json`
5. API `/api/codes?id=v-bcc` 返回时包含关联的文章列表

**响应示例**：
```json
{
  "id": "v-bcc",
  "path": "/code/graph/v-bcc.cpp",
  "language": "cpp",
  "description": "点双连通分量 Tarjan 算法",
  "tags": ["图论", "连通性"],
  "articles": [
    { "id": "graph-bcc", "title": "点双连通分量", "url": "/graph/bcc/index.html" },
    { "id": "graph-adv", "title": "图论进阶", "url": "/graph/advanced/index.html" }
  ],
  "content": "#include <bits/stdc++.h>\n..."
}
```

**为什么这样设计**：
- 在 `code_template_filter` 前端页面中展示"关联文章"列，点击可跳转
- AI agent 可以了解某个代码模板在哪些文章中使用了
- 反向索引在构建期生成，API 查询时 O(1) 读取

---

### 决策 6：`pre_check.ts` 校验系统

**目标**：在构建前扫描所有文件，校验数据完整性。

**校验内容**：

| 校验项 | 级别 | 说明 |
|--------|------|------|
| 所有文章必须有 `id` | ERROR | 缺少则终止构建 |
| 文章 `id` 全局唯一 | ERROR | 重复则终止构建 |
| 文章 `id` 格式符合 `/^[a-z0-9-]+$/` | ERROR | 不符合则终止构建 |
| 所有代码必须有 `id` | ERROR | 缺少则终止构建 |
| 代码 `id` 全局唯一 | ERROR | 重复则终止构建 |
| 代码 `path` 对应文件存在 | ERROR | 不存在则终止构建 |
| 文章引用的 `code_template` ID 在 `code.yaml` 中已注册 | ERROR | 不存在则终止构建 |
| 文章缺少 `title` | ERROR | 缺少则终止构建 |
| 文章缺少 `description` | WARNING | 缺少则警告（可继续） |
| 文章缺少 `tags` | WARNING | 缺少则警告（可继续） |
| `tags` 不是数组 | ERROR | 字符串自动转为数组并警告 |

**实现方式**：
- 校验逻辑抽取到 `@rbook/core/validation` 模块
- `pre_check.ts` 调用校验函数
- 收集所有错误后一次性报告，含文件路径
- 有 ERROR 则 `process.exit(1)`，只有 WARNING 则继续

**报告格式**：
```
[ERROR] book/pages/graph/bcc/index.md: missing required field 'id'
[ERROR] book/pages/graph/scc/index.md: duplicate id 'graph' with graph/bcc/index.md
[WARNING] book/pages/utils/random/index.md: missing field 'description'
```

**集成到构建流程**：
```json
{
  "scripts": {
    "pre-check": "node packages/rbook-core/dist/preCheck.js",
    "build:packages": "tsc -p packages/... && npm run pre-check"
  }
}
```

---

### 决策 7：统一 CLI 客户端（`rbook.py`）

**决策**：删除旧的 `rbook_*.py` 脚本，实现统一的 `rbook.py` CLI。

**命令列表**：
```bash
# 健康检查
rbook health

# 站点信息
rbook site

# 目录（带格式参数）
rbook catalog [--compact]

# 文章操作
rbook pages [--id <id>] [--tag <tag1,tag2>] [--limit 50] [--offset 0]

# 代码操作
rbook codes [--id <id>] [--tag <tag1,tag2>]
rbook code <id> [--content]

# 标签操作
rbook tags
```

**配置优先级**：
1. 命令行参数：`--baseurl https://xxx.com`
2. 环境变量：`RBOOK_BASE_URL`
3. 配置文件：`~/.rbookrc`
4. 默认值：`https://rbook2.roj.ac.cn`

**位置**：`scripts/rbook.py`

---

### 决策 8：迁移策略

**决策**：立即废弃旧 API，只保留新 API（破坏性更新）。

- 删除所有 `/api/ai/*` 路由
- 删除 `/api/toc`、`/api/nav`、`/api/search`、`/api/chunks/search` 等冗余端点
- 删除旧的 Python 脚本
- 旧 rbook-http skill 文档同步更新

---

## 分阶段实施计划

### Phase 1：基础设施与数据准备

**目标**：建立校验系统和数据规范，迁移现有数据。

**任务清单**：

- [ ] **1.1** 在 `@rbook/core` 中创建 `validation` 模块
  - 文件：`packages/rbook-core/src/validation.ts`
  - 函数：`loadCodeConfig()`、`validatePages()`、`validateCodes()`、`validateReferences()`
  - 依赖：`collectPages()` from `@rbook/search`

- [ ] **1.2** 实现 `preCheck.ts`
  - 文件：`packages/rbook-core/src/preCheck.ts`
  - 调用 validation 模块，收集错误并报告，有 ERROR 则 `process.exit(1)`

- [ ] **1.3** 添加构建脚本
  - 在根 `package.json` 添加 `"pre-check": "node packages/rbook-core/dist/preCheck.js"`
  - 修改 `"build:packages"` 加入 `&& npm run pre-check`

- [ ] **1.4** 创建 `book/code.yaml`
  - 扫描 `book/code/` 下的所有代码文件
  - 生成初始代码模板列表（`id`、`path`、`description`、`language`、`tags`）
  - 先手动编写，后续可考虑自动生成脚本

- [ ] **1.5** 批量给所有文章添加 `id`
  - 扫描 `book/pages/` 下所有 `.md` 文件
  - 基于文件路径自动生成 `id`（如 `graph/bcc/index.md` → `graph-bcc`）
  - 可以用脚本辅助，生成后手动检查
  - 修改所有文章的 `code_template` 字段，从对象格式改为 `id` 数组

- [ ] **1.6** 运行验证
  - `npm run pre-check` 必须通过
  - `npm run build:all` 构建成功

**Phase 1 验收标准**：
- [ ] `npm run pre-check` 通过，输出无 ERROR（允许有 WARNING）
- [ ] 所有文章都有唯一的 `id`，格式符合 `/^[a-z0-9-]+$/`
- [ ] `book/code.yaml` 包含所有现有代码模板，每个代码都有 `id`、`path`、`description`
- [ ] 所有文章的 `code_template` 字段已从对象格式改为 ID 数组（例如 `[v-bcc, dijkstra]`）
- [ ] `npm run build:all` 构建成功，生成的 `site/.search/index.json` 包含 `codes` 和 `codeToArticles` 字段
- [ ] 文章引用的所有代码 ID 都在 `code.yaml` 中存在（pre_check 已校验）

---

### Phase 2：API 重构

**目标**：删除旧 API，实现新的简化 API。

**任务清单**：

- [ ] **2.1** 修改 `@rbook/search` 的 `buildIndex`
  - 将 `id` 提升为 `page` 对象的顶级字段
  - 读取 `book/code.yaml`，将代码元数据加入索引
  - 构建 `codeToArticles` 反向映射
  - 索引输出包含 `codes` 和 `codeToArticles` 字段

- [ ] **2.2** 实现新的 API 路由
  - `GET /api/help`（渲染 `docs/api-usage.md`，默认 HTML，支持 `?format=md`）
  - `GET /api/catalog`（支持 `?compact=true`）
  - `GET /api/pages`（支持 `?id=`、`?tag=`、`?limit=`、`?offset=`）
  - `GET /api/codes`（支持 `?id=`、`?tag=`、`?includeContent=`）
  - `GET /api/tags`（返回 `{ articleTags, codeTags }`）
  - 保留但不修改：`GET /api/health`、`GET /api/site`

- [ ] **2.3** 删除旧 API 路由
  - 删除 `packages/rbook-server/src/routes/aiApi.ts` 的全部内容
  - 删除 `publicApi.ts` 中的 `/api/toc`、`/api/nav`、`/api/search`、`/api/chunks/search`、`/api/page`
  - 更新 `packages/rbook-server/src/app.ts` 中的路由注册

- [ ] **2.4** 更新 API 文档页面
  - 修改 `packages/rbook-server/src/docs/apiDocs.ts`
  - 列出新 API 端点、参数说明和 curl 示例

- [ ] **2.5** 测试验证
  - 启动服务器，测试所有新 API 端点
  - 验证错误场景返回正确的错误码和消息
  - 验证分页和标签筛选功能

**Phase 2 验收标准**：
- [ ] 所有新 API 端点正常工作：
  - `GET /api/help` 返回 HTML 和 markdown 两种格式
  - `GET /api/catalog` 返回文章列表，`?compact=true` 返回简化格式
  - `GET /api/pages?id=xxx` 可以通过 ID 查询单篇文章
  - `GET /api/pages?tag=xxx` 可以按标签筛选文章
  - `GET /api/codes` 返回代码列表，包含每个代码的关联文章
  - `GET /api/codes?id=xxx` 返回单个代码详情及其关联文章
  - `GET /api/tags` 返回 `{ articleTags: [...], codeTags: [...] }`
- [ ] 旧 API 端点已全部删除（`/api/ai/*`、`/api/toc`、`/api/nav`、`/api/search`、`/api/chunks/search`）
- [ ] 错误场景返回正确格式：`{ "error": "CODE", "message": "..." }`，HTTP 状态码正确（400/404/500）
- [ ] 访问 `http://localhost:3000/api` 显示更新后的 API 文档
- [ ] 生成的索引中包含 `codes` 和 `codeToArticles` 反向映射

---

### Phase 3：客户端与文档

**目标**：实现统一 CLI，更新文档和前端。

**任务清单**：

- [ ] **3.1** 实现 `scripts/rbook.py`
  - 支持全部子命令：`health`、`site`、`catalog`、`pages`、`codes`、`code`、`tags`
  - 支持 `--baseurl`、`--compact`、`--content` 等参数
  - 支持环境变量 `RBOOK_BASE_URL` 和配置文件 `~/.rbookrc`
  - 输出支持 `--pretty` 格式化 JSON
  - 内部复用 `scripts/rbook_client.py` 的逻辑（如有必要）

- [ ] **3.2** 删除旧的 Python 脚本
  - 删除 `skills/rbook-http/scripts/rbook_health.py`
  - 删除 `skills/rbook-http/scripts/rbook_catalog.py`
  - 删除 `skills/rbook-http/scripts/rbook_search.py`
  - 删除 `skills/rbook-http/scripts/rbook_page_context.py`
  - 删除 `skills/rbook-http/scripts/rbook_code.py`
  - 保留 `rbook_client.py`（如果 `rbook.py` 依赖它）

- [ ] **3.3** 更新文档
  - 更新 `docs/api-usage.md`（API 使用指南）
  - 更新 `skills/rbook-http/SKILL.md`（skill 文档）
  - 删除或更新 `how-to-learn-api.md`（如已过时）
  - 更新 AI API 字段契约（`docs/development/ai-api-schema.md`）

- [ ] **3.4** 更新前端代码模板页面
  - 修改 `site/widgets/code_template_filter/`
  - 展示代码列表，新增"关联文章"列
  - 点击文章链接可以跳转到对应的文章

- [ ] **3.5** 端到端验证
  - `npm run build:all` 构建通过
  - 启动服务器，用 `rbook.py` 测试所有子命令
  - 验证前端页面（目录、文章、代码模板）正常显示
  - 清理构建
  - `npm run dev` 启动开发服务器验证 API

**Phase 3 验收标准**：
- [ ] `scripts/rbook.py` 所有子命令正常工作：
  - `rbook health` 返回健康状态
  - `rbook site` 返回站点信息
  - `rbook catalog` 和 `rbook catalog --compact` 返回不同粒度的目录
  - `rbook pages --id xxx` 查询单篇文章
  - `rbook pages --tag xxx` 按标签筛选
  - `rbook codes` 和 `rbook codes --id xxx` 查询代码
  - `rbook code xxx --content` 返回代码正文
  - `rbook tags` 返回标签统计
- [ ] 旧的 Python 脚本已删除（`rbook_health.py`、`rbook_catalog.py` 等）
- [ ] 文档已更新：
  - `docs/api-usage.md` 反映新 API
  - `skills/rbook-http/SKILL.md` 使用新 API 和 `rbook.py` 示例
  - `docs/development/ai-api-schema.md` 反映新的字段契约
- [ ] 前端代码模板页面显示"关联文章"列，点击可跳转
- [ ] 端到端测试通过：
  - 开发模式（`npm run dev`）和生产模式（`npm run serve`）都能正常运行
  - 访问首页、文章页、代码模板页都正常显示
  - API 所有端点都返回正确数据

---

## 附录

### 现有代码涉及的文件

```
packages/rbook-server/src/
├── app.ts                    # 修改：删除旧路由注册
├── routes/
│   ├── aiApi.ts              # 删除整个文件
│   ├── publicApi.ts          # 修改：删除废弃端点，添加新端点
│   └── adminApi.ts           # 保持不变
├── services/
│   ├── aiContextService.ts   # 删除整个文件
│   ├── codeService.ts        # 修改：从 code.yaml 读取
│   └── pageService.ts        # 修改：支持 id 查询
└── docs/
    └── apiDocs.ts            # 修改：更新端点列表

packages/rbook-core/src/
└── validation.ts             # 新增：校验逻辑
└── preCheck.ts               # 新增：构建前检查

packages/rbook-search/src/
└── buildIndex.ts             # 修改：支持 id 和反向索引

book/
└── code.yaml                 # 新增：代码模板注册表

scripts/
└── rbook.py                  # 新增：统一 CLI
```

### 关键约定速查

| 约定 | 示例 |
|------|------|
| 文章 ID 格式 | `graph-bcc`, `kmp-algo` (小写字母、数字、连字符) |
| 代码 ID 格式 | 与文章 ID 相同 |
| 代码路径 | `path: graph/v-bcc.cpp` (相对于 `book/code/`) |
| frontMatter 引用代码 | `code_template: [v-bcc, dijkstra]` (只存 ID) |
| API 查询单资源 | `?id=xxx` |
| API 筛选标签 | `?tag=图论,动态规划` (逗号分隔，OR 逻辑) |
| API 分页 | `?limit=50&offset=0` |
| 错误格式 | `{ "error": "CODE", "message": "..." }` |
| 响应中链接前缀 | API 返回 `/code/...` 和 `/path/index.html`，调用方拼接 BASE_URL |
