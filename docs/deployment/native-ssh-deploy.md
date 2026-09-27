# rbook 本地构建与 SSH 部署

rbook 的生产服务不再依赖 Docker 或 GitHub Actions 部署。GitHub 保存源码并运行 CI；本机生成与 commit 对应的 release，通过 SSH 上传到 bohai，由 systemd 管理服务。

```text
干净的 main commit
  -> 本地编译、内容检查、runtime 构建
  -> 本地候选服务健康检查
  -> git push origin main
  -> zstd 压缩并由 rsync 上传
  -> VPS 临时端口候选检查
  -> /opt/rbook/current 原子切换
  -> systemd 重启
  -> 本机端口与公网健康检查
  -> 失败时切回上一个 release
```

## 使用

预览部署范围：

```bash
./deploy.sh --dry-run
```

执行部署：

```bash
./deploy.sh
```

脚本要求当前分支为干净的 `main`。尚未 push 的 commit 会在本地构建和检查通过后推送；如果 push 已成功但部署失败，再次执行会重新部署同一个 commit。

默认 SSH alias 是 `bohai`，可临时覆盖：

```bash
RBOOK_DEPLOY_HOST=example ./deploy.sh
```

## 发布内容

每个版本位于：

```text
/opt/rbook/releases/<full-commit-sha>
```

release 包含编译后的服务、`book/`、`docs/` 和预构建 runtime。生产依赖按 `package-lock.json` 的 SHA-256 缓存在 `/opt/rbook/dependencies/`；锁文件不变时不会重复上传依赖。

服务器只保留当前和上一个 release。`/opt/rbook/current` 是指向当前 release 的符号链接。

## 服务管理

服务由 `rbook.service` 管理，以无登录权限的 `rbook` 用户运行：

```bash
ssh bohai 'systemctl status rbook --no-pager'
ssh bohai 'journalctl -u rbook -n 100 --no-pager'
```

生产配置位于 `/opt/rbook/deploy.env`，权限必须为 `600`。默认配置包括：

```text
HOST=0.0.0.0
PORT=3000
RBOOK_ADMIN_TOKEN=...
PCS2_API_BASE_URL=http://127.0.0.1:3300
PCS2_PUBLIC_BASE_URL=https://pcs2.roj.ac.cn
```

PCS2 的容器已将端口映射到宿主机 `127.0.0.1:3300`，原生 rbook 服务通过该地址访问 PCS2。

## 回滚与健康检查

部署在临时端口启动候选服务。切换后依次检查：

```text
http://127.0.0.1:3000/api/health
https://rbook2.roj.ac.cn/api/health
```

本机检查失败时必定回滚。公网地址若在部署前正常、部署后连续失败，也会回滚。回滚会同时恢复应用代码、内容、runtime 和对应依赖。

部署记录追加到 `/opt/rbook/deployments.log`，包含 commit、模式、来源、结果和是否回滚，不记录密钥。

## GitHub Actions

`.github/workflows/ci.yml` 只执行构建和 API 测试，不连接 VPS，也不构建或推送 Docker 镜像。本地部署不等待 CI runner。

Dockerfile 和 Compose 文件暂时保留，供开发或首次迁移后的紧急恢复使用。
