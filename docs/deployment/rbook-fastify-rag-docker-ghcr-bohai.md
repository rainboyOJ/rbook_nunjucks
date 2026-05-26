# rbook Fastify API + Docker + GHCR + bohai 部署预读

本文说明这次把 `rbook_nunjucks` 从单纯静态电子书扩展为“静态站点 + HTTP API 知识查询服务”的改造方案、修改点、部署流程和人工检查项。

## 目标

保留原来的静态电子书构建能力，同时新增一个给 agent 使用的 HTTP API：

```text
Markdown 源文件
  -> 构建 dist/ 静态站
  -> 构建 .search/index.json 本地搜索索引
  -> Fastify 同时服务静态站和 /api/*
  -> Docker 镜像推送到 GHCR
  -> GitHub Actions SSH 到 bohai
  -> bohai 通过 ghcr.nju.edu.cn 拉镜像并重启容器
```

这样浏览器仍然访问原电子书页面，agent 可以通过 HTTP API 查询知识库内容。

## 修改点

新增搜索索引模块：

```text
src/search/paths.js
src/search/collectPages.js
src/search/markdownText.js
src/search/buildIndex.js
src/search/searchIndex.js
```

职责：

- 从 `book.yaml` 的 `chapters` 收集目录可见页面。
- 从 `book.yaml` 的 `glob` 收集隐藏但需要渲染的页面。
- 默认补充扫描 `book/**/*.md`，但过滤草稿、备份、隐藏文件和 TODO。
- 读取 Markdown，处理 `@include_md("...")`。
- 解析 front matter。
- 按标题把文档切成 chunk。
- 使用 `Fuse.js` 构建本地全文检索索引。
- 输出 `.search/index.json`。

新增 Fastify 服务：

```text
src/server/app.js
src/server/serve.js
```

职责：

- 服务 `dist/` 静态文件。
- 提供 `/api/*` 查询接口。
- 支持受 token 保护的重建索引接口。

新增容器和部署文件：

```text
Dockerfile
.dockerignore
docker-compose.yml
.github/workflows/docker-deploy-bohai.yml
```

修改：

```text
package.json
package-lock.json
```

新增依赖：

```text
fastify
@fastify/static
```

新增 npm scripts：

```text
npm run build:index   # 只构建搜索索引
npm run build:all     # 构建静态站 + 搜索索引
npm run serve         # 启动 Fastify 服务
npm run serve:dev     # 保留旧 src/server.js 开发服务器
npm run dev           # 启动 Fastify 服务
```

## API

### 健康检查

```bash
curl http://127.0.0.1:3000/api/health
```

返回索引生成时间和页面/chunk 数量。

### 站点信息

```bash
curl http://127.0.0.1:3000/api/site
```

返回 `book.yaml` 中的标题、作者、描述和索引统计。

### 页面列表

```bash
curl 'http://127.0.0.1:3000/api/pages'
curl 'http://127.0.0.1:3000/api/pages?visible=true'
```

`visible=true` 只返回目录中可见的页面。

### 单页 Markdown

```bash
curl 'http://127.0.0.1:3000/api/page?path=base/binary_search/index.md'
```

返回页面元信息、标题、URL、标题列表和原始 Markdown。

### 页面级搜索

```bash
curl -G \
  --data-urlencode 'q=二分答案' \
  --data-urlencode 'limit=5' \
  http://127.0.0.1:3000/api/search
```

适合 agent 先定位相关页面。

### Chunk 级搜索

```bash
curl -G \
  --data-urlencode 'q=线段树' \
  --data-urlencode 'limit=5' \
  http://127.0.0.1:3000/api/chunks/search
```

适合 agent 直接获取 RAG 上下文。

### 重建索引

如果容器设置了 `RBOOK_ADMIN_TOKEN`：

```bash
curl -X POST \
  -H 'Authorization: Bearer YOUR_TOKEN' \
  http://127.0.0.1:3000/api/admin/reindex
```

如果未设置 `RBOOK_ADMIN_TOKEN`，该接口不鉴权。生产环境建议设置。

## 本地运行

安装依赖：

```bash
npm ci --legacy-peer-deps
```

构建静态站和索引：

```bash
npm run build:all
```

启动服务：

```bash
npm run serve
```

访问：

```text
http://127.0.0.1:3000/
http://127.0.0.1:3000/api/health
```

## Docker 本地验证

构建镜像：

```bash
docker build -t rbook-nunjucks:local .
```

运行：

```bash
docker run --rm -p 3000:3000 rbook-nunjucks:local
```

检查：

```bash
curl http://127.0.0.1:3000/api/health
curl -G --data-urlencode 'q=二分答案' --data-urlencode 'limit=3' http://127.0.0.1:3000/api/search
```

## GitHub Actions 部署

新增 workflow：

```text
.github/workflows/docker-deploy-bohai.yml
```

触发条件：

- push 到 `main`
- 手动 `workflow_dispatch`

流程：

```text
Checkout
  -> docker build
  -> push ghcr.io/<owner>/<repo>:<sha>
  -> push ghcr.io/<owner>/<repo>:latest
  -> SSH 到 bohai
  -> docker pull ghcr.nju.edu.cn/<owner>/<repo>:<sha>
  -> docker rm -f rbook
  -> docker run -d --restart unless-stopped -p HOST_PORT:3000 ...
```

注意：镜像推送到官方 GHCR，bohai 拉取时使用南京大学加速域名：

```text
ghcr.nju.edu.cn/<owner>/<repo>:<sha>
```

这和下面这种形式一致：

```bash
docker pull ghcr.nju.edu.cn/open-webui/open-webui:v0.8.12
```

## GitHub Secrets

在 GitHub 仓库的 `bohai` environment 或仓库级 Actions Secrets 中配置：

```text
VPS_HOST          bohai 的 IP 或域名
VPS_USER          bohai 上可运行 docker 的用户
VPS_SSH_KEY       GitHub Actions 专用 SSH 私钥全文
VPS_SSH_PORT      可选，默认 22
RBOOK_HOST_PORT   可选，默认 3000
RBOOK_ADMIN_TOKEN 可选，保护 /api/admin/reindex
```

`VPS_SSH_KEY` 必须包含完整私钥：

```text
-----BEGIN OPENSSH PRIVATE KEY-----
...
-----END OPENSSH PRIVATE KEY-----
```

## bohai VPS 要求

bohai 上需要：

- 已安装 Docker。
- `VPS_USER` 可以 SSH 登录。
- `VPS_USER` 可以执行 `docker` 命令。
- 服务器防火墙允许 `RBOOK_HOST_PORT`。
- 如果前面有 Nginx，Nginx 反向代理到 `127.0.0.1:RBOOK_HOST_PORT`。

检查 Docker：

```bash
ssh bohai 'docker version'
```

如果部署用户没有 docker 权限：

```bash
sudo usermod -aG docker VPS_USER
```

然后重新登录 SSH 会话。

## Nginx 反向代理示例

如果公网域名继续使用 Nginx，站点可以代理到容器端口：

```nginx
server {
    listen 80;
    server_name rbook2.roj.ac.cn;

    location / {
        proxy_pass http://127.0.0.1:3000;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
    }
}
```

## 人工检查点

第一次合并前建议人工检查：

1. GitHub Packages 是否允许 Actions 写入 package。
2. GHCR 镜像是否公开；如果私有，bohai 需要先 `docker login ghcr.io`。
3. bohai 是否能执行：

   ```bash
   docker pull ghcr.nju.edu.cn/<owner>/<repo>:latest
   ```

4. `RBOOK_HOST_PORT` 是否被占用。
5. 如果旧静态站还在同域名下，Nginx root 和 proxy_pass 不要同时冲突。

## 回滚

查看历史镜像 tag：

```bash
docker images | grep rbook
```

或在 GitHub Packages 页面找到上一个 SHA tag，然后在 bohai 执行：

```bash
docker pull ghcr.nju.edu.cn/<owner>/<repo>:<old-sha>
docker rm -f rbook
docker run -d \
  --name rbook \
  --restart unless-stopped \
  -p 3000:3000 \
  -e NODE_ENV=production \
  -e HOST=0.0.0.0 \
  -e PORT=3000 \
  ghcr.nju.edu.cn/<owner>/<repo>:<old-sha>
```

## 设计取舍

当前版本是轻量 RAG 查询层，不引入向量数据库：

- 优点：部署简单，镜像自包含，bohai 不需要额外服务。
- 优点：算法书内容关键词明确，Fuse 全文搜索第一阶段足够实用。
- 限制：语义召回能力弱于 embedding/vector search。

后续如果需要更强的语义检索，可以在现有 chunk 结构上追加：

```text
chunks -> embeddings -> sqlite-vec/lancedb/qdrant -> /api/semantic-search
```

现有 API 不需要推翻。
