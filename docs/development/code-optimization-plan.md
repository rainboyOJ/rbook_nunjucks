# 代码优化计划

本文档记录算法电子书项目的 TS 代码优化路线。目标不是改变功能，而是把“文章内容”和“项目代码”两部分的边界整理清楚，让代码更适合人阅读、维护和继续扩展。

## 总目标

- 模块化：把超长文件拆成职责明确的小模块。
- 易阅读：减少历史注释、废弃代码和重复逻辑。
- 简洁架构：核心流程保持直线式阅读，细节下沉到命名清晰的函数。
- 中文注释：只在业务意图、路径约定、部署约束等不容易从代码看出的地方补充中文说明。
- 行为稳定：每一步都以小提交完成，并通过 typecheck 和运行时构建验证。

## 已完成阶段

### 阶段 0：服务端 API 模块化

- 提交：`58763cc refactor: modularize rbook server api`
- 内容：
  - 将 `packages/rbook-server/src/app.ts` 中的路由、鉴权、查询参数、服务逻辑拆分到独立模块。
  - 新增 `routes/`、`services/`、`http/`、`docs/` 等目录。
- 验证：
  - `npm run typecheck`
  - `npm run build:runtime`
  - Fastify inject smoke test

## 本轮完成记录

- `0be48f3 docs: add code optimization plan`
  - 写入本文档，明确后续优化阶段、验证命令和提交点。
- `6f238f0 refactor: simplify code template loader`
  - 精简代码模板页面的数据加载器，删除旧 ZIP 和图节点代码。
- `15bdacf refactor: clarify code template book catalog`
  - 用 `bookCatalog.ts` 替代历史命名的 `fake_rbook.ts`。
- `21e43ef refactor: make runtime build pipeline readable`
  - 提取运行时构建中的命令执行和文件遍历 helper。
- `0f6756a refactor: improve rbook core readability`
  - 整理 `@rbook/core` 构建器类型、命名和资源复制入口。
- `059cdfb refactor: tidy code template UI state`
  - 整理代码模板前端组件状态、搜索、下载和复制逻辑。

本轮各阶段均已通过：

- `npm run typecheck`
- `RBOOK_CONTENT_DIR=/home/rainboy/mycode/rbook_nunjucks/book RBOOK_CODE_DIR=/home/rainboy/mycode/rbook_nunjucks/book/code RBOOK_RUNTIME_DIR=/tmp/rbook-runtime-check npm run build:runtime`

## 执行计划

### 阶段 1：整理代码模板页面的数据加载器

- 目标文件：
  - `site/widgets/code_template_filter/load_data.ts`
- 问题：
  - 一个文件同时负责扫描文章、解析 front matter、复制模板代码、生成 Vite 全局变量。
  - 保留了大量旧 ZIP、图节点、调试注释代码。
  - `Markdown` 类只用于读取 front matter，抽象过重。
- 改法：
  - 删除废弃 ZIP 和图节点代码。
  - 提取 `readFrontMatter()`、`resolveTemplateCodePath()`、`copyTemplateCode()`、`createTemplateRecord()`。
  - 将 `load_data()` 改名为语义更清楚的 `loadTemplateRecords()`。
  - 保持 `template_array` 注入行为不变。
- 验证：
  - `npm run typecheck`
  - `npm run build:runtime`
- 提交点：
  - `refactor: simplify code template loader`

### 阶段 2：去掉代码模板页面的 fake rbook 概念

- 目标文件：
  - `site/widgets/code_template_filter/fake_rbook.ts`
  - `site/widgets/code_template_filter/load_data.ts`
- 问题：
  - `fake_rbook.ts` 这个名字表达的是历史限制，不表达当前职责。
  - 它只需要提供 book 配置读取和 Markdown 文件枚举，不应该像完整 rbook 构建器。
- 改法：
  - 新建更明确的 helper，例如 `bookCatalog.ts`。
  - 只暴露 `loadBookConfig()`、`collectMarkdownFiles()`、路径常量。
  - 删除未使用的 assets 拷贝方法。
- 验证：
  - `npm run typecheck`
  - `npm run build:runtime`
- 提交点：
  - `refactor: clarify code template book catalog`

### 阶段 3：整理运行时构建脚本

- 目标文件：
  - `packages/rbook-server/src/buildRuntime.ts`
- 问题：
  - `spawnSync` 错误处理重复。
  - 遍历文件、复制资源、编译 dot 文件的细节混在主流程里。
- 改法：
  - 提取命令执行 helper，例如 `runCommand()`、`hasCommand()`。
  - 提取目录遍历 helper，例如 `walkFiles()`。
  - 保持 `buildRuntime()` 主流程清楚展示构建顺序。
- 验证：
  - `npm run typecheck`
  - `npm run build:runtime`
- 提交点：
  - `refactor: make runtime build pipeline readable`

### 阶段 4：整理 rbook core 构建器

- 目标文件：
  - `packages/rbook-core/src/index.ts`
- 问题：
  - `rbook` 类包含配置读取、文章查找、文章渲染、首页渲染、资源复制等多种职责。
  - 方法命名里仍有历史痕迹，例如 `deal_assets()`。
- 改法：
  - 先做低风险整理：补充类型、删除无效变量、改善命名和中文注释。
  - 如拆分收益明显，再提取 `collectMarkdownFiles()`、`copyAssets()` 等纯函数。
  - 保留对外默认导出，避免破坏已有调用。
- 验证：
  - `npm run typecheck`
  - `npm run build:runtime`
- 提交点：
  - `refactor: improve rbook core readability`

### 阶段 5：整理代码模板前端组件

- 目标文件：
  - `site/widgets/code_template_filter/src/App.vue`
- 问题：
  - 搜索、标签过滤、展示状态集中在单个组件里。
  - 类型约束较弱。
- 改法：
  - 提取数据类型和计算逻辑。
  - 保持界面行为不变，只改善阅读性。
- 验证：
  - `npm run typecheck`
  - `npm run build:runtime`
- 提交点：
  - `refactor: tidy code template UI state`

## 每步提交规则

每个阶段完成后必须：

1. 运行对应验证命令。
2. 查看 `git diff --stat` 和关键 diff。
3. 只提交本阶段相关文件。
4. 保留用户已有未跟踪文件，不纳入提交。
