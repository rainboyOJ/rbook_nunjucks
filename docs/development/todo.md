# rbook 后续优化 TODO

本文档记录下一阶段优化方向。当前优先级不是继续做泛化重构，而是先把“本地智能体通过 HTTP 稳定读取电子书内容”这条主线做扎实。

## 优先级总览

1. AI API 契约测试
2. 部署后健康检查
3. workflow 去重
4. 清理废弃前端代码
5. 清理旧依赖
6. 文档和 skill 与 API 契约同步

## 1. AI API 契约测试

目标：保证 `/api/ai/*` 返回结构稳定，避免再次出现无效绝对链接、字段漂移、代码模板不可读等问题。

### 待办

- [x] 新增 AI API 集成测试脚本。
- [x] 覆盖 `/api/ai/catalog`。
- [x] 覆盖 `/api/ai/page-context?includeCode=true`。
- [x] 覆盖 `/api/ai/code?path=/code/...`。
- [x] 断言 AI API 不返回 `href`、`codeHref` 这类由服务端拼接的绝对链接。
- [x] 断言响应体不包含 `127.0.0.1`。
- [x] 断言 `url`、`codeUrl`、`path` 等相对字段存在。
- [x] 断言非法代码路径，例如 `../package.json`，会被拒绝。
- [x] 将测试接入 `npm run typecheck` 或新增独立 `npm run test:api`。

### 验收标准

- [x] 本地运行测试可以稳定通过。
- [x] CI 中可以自动发现 AI API 字段结构错误。
- [x] `/api/ai/catalog`、`/api/ai/page-context`、`/api/ai/code` 的字段契约有测试保护。

## 2. 部署后健康检查

目标：GitHub Actions 中 deploy 成功必须代表线上服务真正可用，而不是只代表 SSH 脚本执行完。

### 待办

- [x] 在 `deploy-vps.yml` 容器启动后轮询 `/api/health`。
- [x] 在 `content-deploy.yml` 容器启动后轮询 `/api/health`。
- [x] 健康检查要求 HTTP 200。
- [x] 健康检查要求 `ok=true`。
- [x] 健康检查要求 `stats.errors=0`。
- [x] 给容器预编译和启动留出合理等待时间，避免短暂 `502` 导致误判。
- [x] 失败时输出容器日志，便于排查。

### 验收标准

- [x] 部署完成后，线上 `/api/health` 已经可访问。
- [x] 如果运行时构建失败、搜索索引错误、服务没启动，workflow 必须失败。
- [x] 部署失败时能从 Actions 日志中看到关键错误。

## 3. workflow 去重

目标：减少 `deploy-vps.yml` 和 `content-deploy.yml` 中重复的 VPS 部署脚本，降低后续维护成本。

### 待办

- [x] 抽取 VPS 上的通用部署逻辑，例如 `scripts/deploy-vps.sh`。
- [x] 支持“新镜像部署”和“只更新文章内容部署”两个模式。
- [x] 保留当前镜像构建完成后再部署的依赖关系。
- [x] 保留文章-only 修改时不重新构建 Docker 的策略。
- [x] 统一 sparse checkout、容器测试构建、容器启动、健康检查逻辑。
- [x] 更新 GitHub Actions，只负责准备参数和调用脚本。

### 验收标准

- [x] 两个 workflow 中不再维护两份大段重复 shell。
- [x] 修改部署逻辑时只需要改一个脚本。
- [x] Docker 镜像部署和内容部署仍然都能工作。

## 4. 清理废弃前端代码

目标：删除或整理已经不再使用的旧组件和旧入口，降低阅读干扰。

### 待办

- [ ] 确认 `site/widgets/code_template_filter/src/components/tableCell.vue` 是否仍被引用。
- [ ] 如果未使用，删除 `tableCell.vue`。
- [ ] 检查 `site/widgets/code_template_filter/app.js` 是否仍参与构建。
- [ ] 删除代码模板页面中不再使用的旧 UI 逻辑。
- [ ] 清理残留 TODO、旧硬编码链接和调试输出。

### 验收标准

- `rg` 不再找到无效旧组件引用。
- 代码模板页面仍能构建并正常访问。
- 前端目录里不再保留明显废弃的交互代码。

## 5. 清理旧依赖

目标：删除不再使用的依赖，减少安装体积和 Docker 镜像体积。

### 待办

- [ ] 扫描 `package.json` 中未使用的依赖。
- [ ] 重点检查 `archiver`、`ejs`、`vite-plugin-ejs` 等历史依赖是否仍需要。
- [ ] 区分运行时依赖和开发依赖。
- [ ] 删除确认无用的依赖。
- [ ] 更新 `package-lock.json`。
- [ ] 重新运行 typecheck 和 runtime build。

### 验收标准

- 删除依赖后项目仍可构建。
- Docker 构建不缺包。
- 代码模板页、Markdown 渲染、API 服务不受影响。

## 6. 文档和 skill 与 API 契约同步

目标：让文档、skill、API 页面和真实接口保持一致，避免本地智能体按旧字段调用。

### 待办

- [ ] 更新 `skills/rbook-http/SKILL.md`，确保只描述当前 AI API 字段。
- [ ] 检查部署文档中是否残留旧 API 字段。
- [ ] 检查 `/api` 文档页是否需要补充 AI API 相对路径说明。
- [ ] 给 AI API 响应结构写一份简明 schema 文档。
- [ ] 明确说明 AI API 不返回服务端拼接的绝对链接。
- [ ] 明确说明调用方应使用 `BASE_URL + url` 或 `BASE_URL + codeUrl` 拼接链接。

### 验收标准

- 文档中没有要求使用 `citation.href`、`codeHref` 等旧字段。
- skill、API 文档页、代码返回结构一致。
- 本地智能体可以只根据 skill 和 API 文档正确调用 rbook。

## 推荐执行顺序

1. 先完成 AI API 契约测试。
2. 再把契约测试接入 CI。
3. 然后补部署后健康检查。
4. 接着抽取 workflow 重复脚本。
5. 再做废弃前端代码和旧依赖清理。
6. 最后做一轮文档和 skill 对齐检查。

## 每阶段通用验证

每完成一个阶段，至少运行：

```bash
npm run typecheck
RBOOK_CONTENT_DIR=/home/rainboy/mycode/rbook_nunjucks/book RBOOK_CODE_DIR=/home/rainboy/mycode/rbook_nunjucks/book/code RBOOK_RUNTIME_DIR=/tmp/rbook-runtime-check npm run build:runtime
```

涉及部署时，还需要验证线上：

```bash
curl https://rbook2.roj.ac.cn/api/health
curl https://rbook2.roj.ac.cn/api/ai/catalog
```
