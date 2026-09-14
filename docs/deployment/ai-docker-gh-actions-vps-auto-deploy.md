# rbook Docker + GitHub Actions + bohai 部署

rbook 使用 GitHub Actions 构建 Docker 镜像，并通过 SSH 在 bohai 上部署。文章内容和运行时代码分开发布：

```text
运行时代码 / Docker 配置
  -> Build Docker Image
  -> ghcr.io/<owner>/<repo>:<commit-sha12> 与 :latest
  -> Deploy to VPS
  -> bohai 从 ghcr.nju.edu.cn/ghcr.io 拉取 SHA 镜像并重启 rbook

book/ 或 docs/
  -> Deploy Content to VPS
  -> bohai sparse-checkout 最新内容
  -> 复用当前镜像并重启 rbook
```

## Workflow

```text
.github/workflows/docker-build.yml       # 构建并推送镜像
.github/workflows/deploy-vps.yml         # 镜像构建成功后部署
.github/workflows/content-deploy.yml     # 只更新 book/ 和 docs/
deploy.sh                                 # 本地校验、push、等待全部 workflow
scripts/deploy-vps.sh                     # 上传到 VPS 后执行的部署脚本
```

`deploy.sh` 只允许干净的 `main` 分支运行。它会运行 `typecheck`、runtime 构建和 API 测试，校验通过后执行 `git push --no-verify`，然后等待该 commit 触发的全部部署 workflow。

混合提交（同时修改内容和运行时代码）会触发两条部署链路。VPS 脚本使用同一把 `flock` 锁串行化部署，避免两个容器重启相互覆盖。

## GitHub Secrets

在仓库的 `bohai` Environment 中配置：

```text
VPS_HOST          bohai 的 IP 或域名
VPS_USER          可 SSH 登录并运行 docker 的用户
VPS_SSH_KEY       GitHub Actions 专用 SSH 私钥全文
VPS_SSH_PORT      可选，默认 22
RBOOK_HOST_PORT   可选，默认 3000
RBOOK_ADMIN_TOKEN 可选，保护管理 API
```

不要把私钥、密码或 VPS 地址写入仓库。`VPS_SSH_KEY` 必须包含完整的 OpenSSH 私钥头尾。

## VPS 约定

默认值如下：

```text
内容 checkout: /opt/rbook/rbook_nunjucks
内容分支:     main
rbook 容器:   rbook
rbook 端口:   3000:3000
Docker 网络:  rbook-services
PCS2 容器:    problems-solution
PCS2 内网 API: http://problems-solution:3000
PCS2 公网链接: https://pcs2.roj.ac.cn
```

内容 checkout 由脚本完全管理，使用 sparse-checkout 保留 `book/`、`docs/`，每次部署重置到 `origin/main`。不要在该目录手工编辑文件。

部署脚本会在网络不存在时创建 `rbook-services`，并在 PCS2 容器存在时把它接入该网络。PCS2 容器不存在只会产生告警，不会阻断 rbook 基础部署。

首次部署前可以手动确认：

```bash
ssh bohai 'docker network inspect rbook-services'
ssh bohai 'docker inspect problems-solution --format "{{json .NetworkSettings.Networks}}"'
```

如果前面有 Nginx，继续反向代理到 `127.0.0.1:3000`，不要新增公网端口。

## 手动恢复

镜像同时保留不可变的 12 位 commit SHA 标签和 `latest`。bohai 使用 `ghcr.nju.edu.cn/ghcr.io/<owner>/<repo>:<tag>` 拉取镜像。部署失败不会自动回滚；可以在 GitHub Actions 手动运行 `Deploy to VPS`，并通过 `image_tag` 指定之前的 SHA：

```text
image_tag = 6f114a37afa0
```

VPS 健康检查访问 `http://127.0.0.1:3000/api/health`。rbook 自身健康检查失败会让 workflow 失败；PCS2 连通性只影响文章题单并记录告警，不阻断 rbook 部署。
