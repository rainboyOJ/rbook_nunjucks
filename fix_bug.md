# Review 问题修复方案

## 文档状态

- 状态：全部修复完成，待提交
- 范围：记录本次 Review 提出的 6 个问题及建议修复方案
- P1-1 已完成：49 篇文章的 front matter 已修复；P1-2 已完成：代码模板页面改为读取 `book/code.yaml`
- P1-3 已完成：仓库内 `nvim/` 插件副本已删除；P1-4 已完成：API 文档、实现与合约测试已统一
- P2-1 已完成：pre-check 已成为全部构建、索引和开发入口的强制门禁
- P2-2 已完成：文章与代码模板统一采用严格字符串 ID 契约
- 修复时必须保留当前 `book/book.yaml` 中的新旧导航入口，不借机调整文章目录结构

## 总体结论

Review 提出的 6 个问题已全部修复。文章元数据、代码模板页面、仓库内 Neovim 插件、API 合约、构建前强制校验和 ID 类型约束均已完成实现与回归验证。

建议按以下顺序处理：

1. 修复 49 篇文章的 front matter，先恢复可信的内容数据。已完成。
2. 收紧文章 ID 校验，并把 pre-check 接入所有真实构建入口。已完成。
3. 修复网页代码模板加载路径。已完成。
4. 删除已经合并到个人配置中的仓库内 Neovim 插件副本。已完成。
5. 按新 API 重写合约测试并明确缓存策略。已完成。

当前合法内容已经通过 pre-check、类型检查、静态构建、runtime 构建、API 合约和代码模板界面验证，可以进入提交审核阶段。

## P1-1：49 篇文章的 front matter 被破坏

### 执行状态

已完成。

- 新增 `scripts/fix-malformed-frontmatter.mjs`，默认 dry-run，只有显式传入 `--write` 才修改文件。
- 新增 `scripts/fix-malformed-frontmatter.test.mjs`，覆盖普通文件、字段较多文件、ID 冲突和非目标文件。
- 49 个损坏文件已合并为单一 front matter，原元数据字段与 Markdown 正文保持不变。
- 其中 44 个文件的两个迁移 ID 一致；另外 5 个冲突通过全库 ID 占用确认后采用顶部合成块中的最终唯一 ID，并在脚本中显式登记。
- 全部 462 篇 Markdown 文章的 ID 均为非空字符串且全库唯一。

### 修复前现象

以 `book/pages/about.md` 为例，文件当时包含两个相邻的元数据区域：

```yaml
---
id: about
title: about
---

---id: about

title: 关于本书
---
```

修复前可用下面的命令找到 49 个同类文件；修复后该命令输出 0：

```bash
rg -l '^---id:' book/pages | wc -l
```

`gray-matter` 只会读取第一个 YAML 块，因此原有的 `title`、`tags`、`categories`、`code_template` 等字段会落入 Markdown 正文。直接表现包括：

- `/api/pages?id=about` 返回错误标题，并把原元数据当正文返回。
- 文章与代码模板之间的反向关联丢失，例如 `fraction` 的关联文章变为空。
- 页面上可能直接显示 YAML 字段。

### 根因

批量迁移 ID 时，没有把 `id` 合并到原 front matter，而是先生成了一个新的 YAML 块，并把原来的起始标记错误改成了 `---id: ...`。

### 修复方案

编写一次性迁移脚本，针对 `^---id:` 文件进行结构化修复：

1. 读取第一个合成 YAML 块，保存其中的 `id`。
2. 把后面的 `---id: <id>` 识别为被破坏的原 front matter 起始位置。
3. 删除合成 YAML 块。
4. 恢复原 front matter 的 `---` 起始标记，并把保存的 `id` 合并进去。
5. 保留原有的 `title`、`date`、`tags`、`categories`、`code_template` 及未知字段，不用文件名生成的新值覆盖原值。
6. 如果合成 ID 与 `---id:` 后的 ID 不一致，停止处理并报告冲突。只有确认候选 ID 已被其他文章占用、合成 ID 全库唯一后，才把最终 ID 加入显式冲突表；不能按路径或顺序自动猜测。
7. 迁移脚本先支持 dry-run，列出待修改文件、合并后的 ID 和异常文件，再执行写入。

`about.md` 的目标结构应为：

```yaml
---
id: about
title: 关于本书
---
```

不能只把 `---id:` 文本替换成 `---`。这样仍会保留第一个合成 YAML 块，`gray-matter` 仍然读不到原元数据。

### 验收标准

- [x] `rg -l '^---id:' book/pages` 无输出。
- [x] 每个修复文件都能被 `gray-matter` 解析，且只存在一个顶部 front matter。
- [x] 修复前原有的元数据字段和值全部保留。
- [x] 所有文章 ID 非空且唯一。
- [x] `/api/pages?id=about` 返回 `title: 关于本书`，正文不再包含原始 YAML。
- [x] 使用 `fraction` 检查 `codeToArticles`，反向关联恢复到 `tricks-fraction-class`。
- [x] 迁移脚本的普通文件、字段较多文件、ID 冲突文件、非目标文件四类测试全部通过。
- [x] `npm run pre-check` 通过：`pages=432`、`codes=189`、`errors=0`；现存缺少 description/tags 的 warning 不属于 P1-1。
- [x] `npm run build:packages` 和隔离目录中的 `npm run build:runtime` 通过；搜索索引生成 `pages=432`、`chunks=4385`、`errors=0`。

## P1-2：网页代码模板加载了错误的 `code.yaml`

### 执行状态

已完成。

- `loadCodeConfig()` 新增严格模式和可测试的 `codeYamlPath` 入口，默认数据源仍是 `contentDir/code.yaml`。
- `site/widgets/code_template_filter/load_data.ts` 已删除 `__workdir + 'book/code.yaml'` 硬编码，直接复用 `@rbook/core` 的 loader。
- widget 构建会校验代码配置，并拒绝缺失文件、目录项、`../` 越界路径和解析到 `book/code` 外部的符号链接。
- 新增 core loader 的 4 项单元测试，以及 widget 真实构建和 6 类失败场景共 7 项端到端测试。

### 现象

`site/widgets/code_template_filter/load_data.ts` 修复前使用：

```ts
path.resolve(__workdir, 'book/code.yaml')
```

而 `__workdir` 在 `bookCatalog.ts` 中等于 `appDir`，也就是仓库的 `site/` 目录。实际查找路径因此是不存在的 `site/book/code.yaml`。加载函数随后静默返回空数组，最终生成的页面注入 `template_array = []`。

### 根因

代码把前端应用目录当成了内容根目录，而且“配置不存在”和“配置确实为空”都被处理成空数组，使完整构建无法暴露错误。

### 修复方案

1. 删除 `load_data.ts` 内重复的路径拼接和 YAML 读取逻辑。
2. 优先复用 `@rbook/core` 的 `loadCodeConfig()`；该函数已经以 `contentDir/code.yaml`，即 `book/code.yaml` 为数据源。
3. 为构建场景提供严格模式：文件不存在、YAML 解析失败、`codes` 不是数组时直接抛错，不能静默返回 `[]`。
4. `load_data.ts` 只负责把统一的代码记录转换成前端需要的字段，并复制 `book/code/` 下的源码。
5. 保留路径越界检查，确保 `item.path` 只能解析到 `book/code/` 内部。

本次采用共享 loader 的严格模式，避免网页、搜索索引和校验器以后再次采用不同数据源。

### 验收标准

- [x] 构建读取根目录下的 `book/code.yaml`，不再访问 `site/book/code.yaml`。
- [x] 缺失或破坏 `book/code.yaml` 后，代码模板构建以非零状态失败并给出真实路径。
- [x] 缺失代码文件、目录穿越及解析到 `book/code` 外部的路径会使构建失败。
- [x] 真实配置读取 189 个模板，构建产物包含 `dsu-on-tree-color-count` 和 `tricks-fraction-class`，不再注入空数组。
- [x] `dsu-on-tree-color-count` 对应源码已发布到构建产物的 `code/tree/dsu_on_tree_color_count.cpp`。
- [x] core loader 单元测试 4/4、widget 端到端测试 7/7 通过，包含目录项和逃逸符号链接。
- [x] 新增 `npm run test:code-template`，保证测试前先编译最新 packages。
- [x] `npm run typecheck` 通过。
- [x] 隔离目录中的 `npm run build:runtime` 通过；搜索索引 `pages=432`、`chunks=4385`、`errors=0`。

## P1-3：删除仓库内已不再维护的 Neovim 插件

### 原问题

文章已迁移到新的引用格式：

```yaml
code_template: [dsu-on-tree-color-count]
```

但 `nvim/lua/rbook/scanner.lua` 仍只接收旧对象：

```lua
if type(item) == "table" and item.code then
```

字符串 ID 会被全部跳过，所以 `Rbook Code Templates` 主入口为空。

### 处理决定

该插件已经合并到用户的个人 Neovim 配置中，仓库内副本不再作为独立插件维护。因此不再迁移此处 scanner，也不在本仓库保留两份可能继续分叉的实现。

### 删除操作

执行状态：已按确认完成。

删除仓库根目录下的整个 `nvim/` 目录，包括：

- `nvim/README.md`
- `nvim/plugin/rbook.lua`
- `nvim/lua/rbook/` 下的 scanner、picker、doctor、catalog、actions、config、deps、paths、init 等模块

删除前已确认：

- `nvim/` 内文件均为已跟踪且没有未提交修改的仓库副本。
- 仓库其他程序代码没有引用 `nvim/`、`rbook.nvim` 或 `RbookCode*` 命令。
- 本操作不读取、修改或删除用户个人 Neovim 配置中的合并版本。

以后关于字符串模板 ID 的适配应在个人 Neovim 配置仓库中完成，不再作为本项目的构建或发布责任。

### 完成审计

- 仓库中的 `nvim/` 目录已经不存在，原有 11 个已跟踪插件文件均处于删除状态，共删除 1047 行。
- 全仓搜索 `rbook.nvim`、`RbookCode`、`nvim/lua/rbook` 和 `nvim/plugin/rbook` 均无项目代码引用。
- `package.json`、`package-lock.json`、CLI、packages、site、scripts、源码和 CI 配置均无 Neovim 插件依赖。
- `npm run build:all` 通过：生成 432 个索引页面、4385 个分块、189 个代码模板，错误为 0。
- `npm run test:api` 已通过，确认 API 合约测试和服务端实现均不依赖被删除的 Neovim 插件。

### 验收标准

- [x] 仓库中不再存在 `nvim/` 目录。
- [x] `rg -n 'rbook\.nvim|RbookCode|nvim/lua/rbook' --glob '!fix_bug.md' .` 不再找到项目代码引用。
- [x] `npm run typecheck`、`npm run build:all` 和 `npm run test:api` 不依赖被删除的插件。
- [x] 用户个人 Neovim 配置不属于本仓库验收范围，由其所在配置仓库单独维护和测试。

## P1-4：API 合约测试仍请求已删除的路由

### 执行状态

已完成。

- 合约脚本已迁移到 `scripts/test-api-contract.mjs`，并在临时 runtime 目录中重建搜索索引后再创建 Fastify 应用，避免读取陈旧的 `site/.search/index.json`。
- `packages/rbook-server/src/app.ts` 通过统一的 `onRequest` hook 为 `/api`、`/api?...` 和 `/api/*` 的成功及错误响应设置 `Cache-Control: no-store`。
- `docs/api-usage.md` 成为 API 文档的唯一内容源；`/api` 和 `/api/help` 渲染该 Markdown，`/api/help?format=md` 严格返回原文。
- 新合约覆盖所有公共接口、文章和代码筛选分页、反向关联、稳定错误码、旧路由 404、根相对 URL 和本机信息防泄漏。
- 未恢复 `/api/md` 或 `/api/ai/*` 兼容路由。

### 现象

`scripts/test-ai-api-contract.mjs` 仍请求以下已删除接口：

- `/api/md`
- `/api/ai/catalog`
- `/api/ai/page-context`
- `/api/ai/code`

测试还要求 `Cache-Control: no-store`。当前新 API 没有统一设置该响应头，所以测试首先在缓存头断言处失败，之后还会继续遇到 404 和旧响应结构断言。

### 根因

服务端已迁移到 `/api/help`、`/api/pages`、`/api/codes` 等公共 API，但合约测试仍描述旧 AI API，测试名称、请求路径、字段断言和错误响应都没有同步更新。

### 修复方案

1. 将脚本重命名为 `scripts/test-api-contract.mjs`，并更新 `package.json` 的 `test:api`。
2. 以 `docs/api-usage.md` 和 `packages/rbook-server/src/routes/publicApi.ts` 定义的新接口为准，删除旧 AI API 专属断言。
3. 建议所有 `/api` 与 `/api/*` 响应统一使用 `Cache-Control: no-store`，避免开发模式修改文章后读到旧内容；通过 Fastify hook 集中设置，不在每个 handler 内重复。
4. 若生产环境决定使用缓存，则必须先明确每类接口的缓存策略，再同步修改测试。不能保留当前“实现无响应头、测试要求 no-store”的不一致状态。
5. API 文档只保留 `docs/api-usage.md` 一个内容源：`/api` 或 `/api/help` 的 HTML 从该 Markdown 渲染，`/api/help?format=md` 返回原始 Markdown，避免在 `apiDocs.ts` 重复硬编码接口说明。

新合约至少覆盖：

- `GET /api`：返回 HTML 文档，并使用请求的 host/protocol 生成示例地址。
- `GET /api/help?format=md`：返回 `text/markdown`，内容等于 `docs/api-usage.md`。
- `GET /api/health`：返回 `ok`、`generatedAt` 和 `stats`。
- `GET /api/catalog?compact=true`：只返回可见文章的精简目录。
- `GET /api/pages?id=<id>`：返回单篇文章的 Markdown、HTML、frontMatter 和模板 ID。
- `GET /api/pages?tag=...&limit=...&offset=...`：验证筛选与分页。
- `GET /api/codes?id=<id>&includeContent=true`：返回模板元数据、源码和反向文章关联。
- `GET /api/tags`：返回文章与代码标签统计。
- 不存在的文章和模板分别返回 404 及稳定错误码 `PAGE_NOT_FOUND`、`CODE_NOT_FOUND`。
- 未注册 `/api/*` 路由返回 `API_ROUTE_NOT_FOUND`。
- 返回的站内 URL 保持根相对路径，不泄漏 `127.0.0.1` 或本机绝对路径。

### 验收标准

- [x] `npm run test:api` 通过。
- [x] 测试文件中不再出现 `/api/md` 或 `/api/ai/`。
- [x] 每个新公共接口至少有一个成功用例；带 ID 的接口至少有一个 404 用例。
- [x] 缓存策略在实现、文档和测试中一致。
- [x] 测试断言新接口的真实字段，不通过兼容层恢复已删除接口来迁就旧测试。
- [x] `npm run typecheck` 通过。
- [x] `npm run build:all` 通过：`pages=432`、`chunks=4385`、`codes=189`、`errors=0`。

## P2-1：pre-check 没有接入真实构建入口

### 执行状态

已完成。

- `runPreCheck()` 现在只读取并校验内容，返回 `{ errors, warnings, stats, ok }`，不输出日志、不修改 `process.exitCode`。
- `reportPreCheck()` 负责输出，CLI 包装器只根据 `ok` 决定退出码；`evaluatePreCheck()` 支持无进程副作用的单元测试。
- `package.json` 新增 compile-only 内部脚本；公开的 `build:packages`、`build`、`build:all`、`build:index`、`build:runtime` 和 `dev` 均强制经过 pre-check。
- `buildRuntime()` 与 `dev.js` 自身也在清理 runtime 目录前调用 `assertPreCheck()`，直接调用编译产物不能绕过门禁。
- 搜索索引写入前会再次使用共享校验规则，页面解析错误、非法 ID 或未知模板引用都不会生成索引。
- `site/build.sh` 现在尊重 `RBOOK_APP_DIR`，构建门禁测试可把 app/content/runtime 产物全部隔离到临时目录。
- 新增 `scripts/pre-check.test.mjs` 和 `scripts/build-gates.test.mjs`。

### 现象

`package.json` 只新增了独立命令：

```json
"pre-check": "node packages/rbook-search/dist/preCheck.js"
```

`build:packages`、`build:all`、`dev`、`build:runtime` 及 `buildRuntime()` 都不会强制执行它。缺失或重复 ID、无效模板引用仍可进入索引和部署产物。

### 根因

校验器作为可选工具存在，没有成为构建管线的 fail-fast 阶段。另外，当前 `runPreCheck()` 同时负责收集问题、输出日志和修改 `process.exitCode`，不适合被多个程序入口安全复用。

### 修复方案

1. 将 pre-check 拆成可复用的纯校验函数和 CLI 包装器：
   - 核心函数返回 `{ errors, warnings, stats, ok }`，不修改进程状态。
   - CLI 根据 `ok` 决定退出码。
   - `buildRuntime()` 等程序入口在 `ok=false` 时抛错并停止。
2. 将当前 TypeScript 编译命令提取为内部的 compile-only 脚本，再让公开的 `build:packages` 执行“编译 -> pre-check”。这样校验脚本运行时已有最新的 dist 文件。
3. `npm run build`、`npm run build:all` 和 `npm run dev` 必须经过已校验的 `build:packages`。
4. 独立执行 `npm run build:index` 时也必须先执行 pre-check，不能成为绕过入口。
5. `buildRuntime()` 在 `resetRuntimeDir()` 和任何产物写入之前直接调用共享校验函数，保证 CLI 之外的程序调用也无法绕过。
6. `dev` 在监听端口前做一次完整启动校验。文章热更新阶段可只校验当前请求的文章，但下一次启动仍必须完整校验。
7. 保持 warning 只提示、不阻止构建；任何 ERROR 必须产生非零退出码，并且不生成或覆盖部署产物。

构建入口的目标行为如下：

| 入口 | 必须校验的时机 |
| --- | --- |
| `npm run build:packages` | TypeScript 编译完成后 |
| `npm run build` | 生成站点前 |
| `npm run build:all` | 生成站点和索引前，允许复用同一次结果 |
| `npm run build:index` | 写索引前 |
| `npm run build:runtime` / `buildRuntime()` | 清理 runtime 目录前 |
| `npm run dev` | 构建前端资源和监听端口前 |

### 验收标准

- [x] 临时制造重复文章 ID 后，上表所有可产生产物或启动服务的入口都会失败。
- [x] 临时制造未知 `code_template` ID 后，同样在写产物前失败。
- [x] 两类错误场景下 app/runtime 哨兵文件保持原内容，并且没有生成 `.search/index.json`。
- [x] warning 不改变退出码；真实内容以 `warnings=666`、`errors=0` 成功通过。
- [x] `buildRuntime()` 和 `dev.js` 校验失败时不会先删除已有 runtime 产物。
- [x] `npm run build:all` 在合法内容下正常通过，不因脚本依赖顺序找不到 `dist/preCheck.js`。
- [x] 共享校验函数单元测试 4/4、构建门禁测试 4/4 通过；门禁测试内部执行了 12 次公开入口失败验证。

## P2-2：非字符串文章 ID 能通过校验但无法查询

### 执行状态

已完成。

- `parsePublicId()` 成为文章、代码模板、pre-check 和索引器共享的唯一 ID 语法规则，不进行任何隐式类型转换。
- `requirePageId()` 与 `requireCodeId()` 为索引阶段提供第二道断言，非法值会携带内容路径抛错。
- `validatePages()` 保留 front matter 中显式写入的空值，不再通过 `fm.id || page.id` 掩盖错误。
- `buildSearchIndex()` 在构造 `pages`、`codes` 和 `codeToArticles` 前完成校验，非法 ID 不会降级成空字符串。
- 新增 `scripts/id-contract.test.mjs`，覆盖类型、格式、唯一性、索引写入和 ID 查询。

### 现象

YAML 中的下面写法会被 `js-yaml` 解析为 number：

```yaml
id: 123
```

`packages/rbook-core/src/validation.ts` 当前把该值传给正则，JavaScript 会隐式转换为字符串，因此校验通过。随后 `packages/rbook-search/src/buildIndex.ts` 的 `getPageId()` 只接受 string，将这个 ID 写成空字符串，导致 `/api/pages?id=123` 永远找不到文章。

### 根因

校验器允许隐式类型转换，而索引器采用严格类型判断，两者对 ID 的数据契约不一致。

### 修复方案

采用“拒绝非字符串”策略，不在索引阶段隐式规范化。文章 ID 是公开 API 标识符，自动把 number 转为 string 会掩盖 YAML 写法错误。

1. `validatePages()` 先检查 `typeof id === 'string'`，类型不符时报告 ERROR，并跳过正则和重复检查。
2. 字符串 ID 再依次检查非空、`/^[a-z0-9-]+$/` 和唯一性。
3. 合法的纯数字字符串必须显式加引号，例如 `id: "123"`。
4. `getPageId()` 不再用空字符串吞掉非法值。共享同一 ID 读取/断言函数，或在遇到非字符串时抛出包含文章路径的错误，形成第二道保护。
5. `getPageById()` 和索引中的 `codeToArticles` 都只使用已经通过校验的字符串 ID。
6. 对 `book/code.yaml` 的代码 ID 应采用同样的运行时类型规则，避免以后出现对称问题。

### 验收标准

- [x] `id: 123` 校验失败，错误包含文件路径和“文章 ID 必须是字符串”。
- [x] `id: "123"` 校验通过，可写入索引并由 `getPageById("123")` 查询；API 使用同一查询函数。
- [x] `id: ""`、空白 ID、重复 ID、含大写字母或非法字符的 ID 校验失败。
- [x] 数字 ID 在创建 `.search/index.json` 前中止，不会以 `id: ""` 进入索引。
- [x] 数字代码模板 ID 以“代码 ID 必须是字符串”失败。
- [x] ID 契约测试 6/6 通过，验证校验器和索引器使用同一规则。

## 回归验证清单

全部修复完成后，至少执行：

```bash
rg -n '^---id:' book/pages
npm run pre-check
npm run typecheck
npm run build:all
npm run build:runtime
npm run test:api
```

还需要进行一项人工验证：

1. 打开 `/code_template/`，搜索并查看一个已知模板及其关联文章。

### 最终执行结果

- [x] `rg -n '^---id:' book/pages` 无匹配。
- [x] `npm run pre-check` 通过：`pages=432`、`codes=189`、`errors=0`、`warnings=666`。
- [x] `npm run typecheck` 通过。
- [x] `npm run build:all` 通过：`pages=432`、`chunks=4385`、`codes=189`、`errors=0`。
- [x] `npm run build:runtime` 通过：`pages=432`、`chunks=4385`、`errors=0`。
- [x] `npm run test:api` 通过。
- [x] `npm run test:code-template` 通过，11/11。
- [x] front matter 修复脚本测试 4/4、ID 与 pre-check 单元测试 10/10、构建门禁测试 4/4 通过。
- [x] 浏览器打开 `/code_template/`，搜索 `dsu_on_tree_color_count`；确认关联文章为“树上启发式合并”，链接为 `/algorithm/dsu_on_tree/index.html`，并成功打开包含 `struct DsuOnTree` 的 2982 字符源码。

## 非目标与风险控制

- 本轮不恢复已删除的 `/api/ai/*` 接口，也不通过兼容路由让旧测试假通过。
- 本轮直接删除仓库内 `nvim/` 插件副本，不迁移或修复其 scanner；个人 Neovim 配置中的合并版本不在本仓库范围内。
- front matter 修复只恢复元数据结构，不改写文章正文，不重新生成标题或标签。
- 不删除 `book/book.yaml` 中为人工复核而保留的新旧入口。
- 批量修复脚本必须先 dry-run；出现 ID 冲突、无法解析或结构不符合预期的文件时停止并交由人工处理。
- 修复过程中不要把当前大量未提交改动整体格式化或回滚，只提交与对应问题直接相关的文件。
