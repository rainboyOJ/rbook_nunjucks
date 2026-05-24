# GitHub Actions + VPS 自动部署 rbook_nunjucks

本项目当前不使用 Docker 镜像部署。每次 push 到 `main` 后，GitHub Actions 在 CI 中构建静态站点，并把 `dist/` 同步到 bohai VPS 上已有 Nginx 的站点目录：

```text
/www/wwwroot/rbook2.roj.ac.cn
```

部署流程：

```text
git push
  -> GitHub Actions 安装依赖
  -> npm run build 生成 dist/
  -> SSH 到 bohai
  -> rsync --delete 同步 dist/ 到 /www/wwwroot/rbook2.roj.ac.cn
  -> VPS 上已有 Nginx 直接对外提供静态站点
```

## 项目文件

核心文件：

```text
.github/workflows/deploy-vps.yml
```

这个 workflow 会：

- 在 push 到 `main` 时自动执行，也支持手动 `workflow_dispatch`。
- 使用 Node.js 22。
- 安装 `graphviz`，用于构建 dot 图。
- 安装 `rsync`，用于同步静态文件。
- 执行 `npm ci` 和 `npm run build`。
- 通过 SSH 连接 bohai。
- 创建目标目录并同步 `dist/`。

## GitHub Secrets

在 GitHub 仓库的 `bohai` environment 或仓库级 Actions Secrets 中配置：

```text
VPS_HOST       = bohai 的 IP 或域名
VPS_USER       = 有 /www/wwwroot/rbook2.roj.ac.cn 写权限的用户
VPS_SSH_KEY    = GitHub Actions 专用 SSH 私钥全文
VPS_SSH_PORT   = SSH 端口，可选，不配置时默认 22
VPS_DEPLOY_DIR = 可选，默认 /www/wwwroot/rbook2.roj.ac.cn
```

`VPS_SSH_KEY` 必须是私钥全文，包括：

```text
-----BEGIN OPENSSH PRIVATE KEY-----
...
-----END OPENSSH PRIVATE KEY-----
```

## VPS 要求

bohai 上需要：

- 已安装并运行 Nginx。
- 已安装 `rsync`。
- Nginx 站点 root 指向 `/www/wwwroot/rbook2.roj.ac.cn`。
- `VPS_USER` 可以 SSH 登录。
- `VPS_USER` 对 `/www/wwwroot/rbook2.roj.ac.cn` 有写权限。

如果缺少 `rsync` 或目录权限不对，在 VPS 上执行类似命令：

```bash
sudo apt-get update
sudo apt-get install -y rsync
sudo mkdir -p /www/wwwroot/rbook2.roj.ac.cn
sudo chown -R VPS_USER:VPS_USER /www/wwwroot/rbook2.roj.ac.cn
```

把 `VPS_USER` 替换成实际部署用户。

## SSH Key 初始化

在本地生成部署 key：

```bash
ssh-keygen -t ed25519 -C "github-actions-rbook-nunjucks" -f ./github_actions_rbook_nunjucks -N ""
```

把公钥放到 bohai 的部署用户：

```bash
ssh-copy-id -i ./github_actions_rbook_nunjucks.pub VPS_USER@VPS_HOST
```

把私钥内容配置到 GitHub Secret `VPS_SSH_KEY`。

## 手动验证

本地验证构建：

```bash
npm ci
npm run build
```

VPS 上验证 Nginx 目录：

```bash
ls -la /www/wwwroot/rbook2.roj.ac.cn
curl -I http://127.0.0.1/
```

GitHub Actions 成功后，检查站点目录是否已经更新：

```bash
find /www/wwwroot/rbook2.roj.ac.cn -maxdepth 2 -type f | head
```

## 注意事项

- `rsync --delete` 会删除目标目录中 `dist/` 不再包含的文件。
- 不要把私钥、VPS IP、真实密码提交到仓库。
- 如果 bohai 的 SSH 端口不是 22，配置 `VPS_SSH_PORT`。
- 如果目标目录未来变化，配置 `VPS_DEPLOY_DIR`，不用改 workflow。
