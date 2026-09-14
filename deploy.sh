#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
BRANCH="main"
DISCOVERY_TIMEOUT="${DEPLOY_DISCOVERY_TIMEOUT:-120}"
WAIT_TIMEOUT="${DEPLOY_WAIT_TIMEOUT:-1800}"
WORKFLOW_IMAGE="Build Docker Image"
WORKFLOW_IMAGE_DEPLOY="Deploy to VPS"
WORKFLOW_CONTENT="Deploy Content to VPS"

die() {
  echo "[deploy] $*" >&2
  exit 1
}

usage() {
  cat <<'EOF'
Usage: ./deploy.sh

Require a clean main branch, run the local deployment checks, push the current
commit, and wait for every GitHub Actions deployment workflow triggered by it.

Environment variables:
  DEPLOY_DISCOVERY_TIMEOUT  Seconds to wait for workflow runs to appear (120)
  DEPLOY_WAIT_TIMEOUT       Seconds to wait for each workflow to finish (1800)
EOF
}

if [[ "${1:-}" == "-h" || "${1:-}" == "--help" ]]; then
  usage
  exit 0
fi

cd "$ROOT_DIR"

for command_name in git gh npm timeout; do
  command -v "$command_name" >/dev/null 2>&1 || die "缺少命令: $command_name"
done

git rev-parse --show-toplevel >/dev/null 2>&1 || die "当前目录不是 Git 仓库"
[[ "$(git branch --show-current)" == "$BRANCH" ]] \
  || die "当前分支必须是 $BRANCH"

assert_clean() {
  local status
  status="$(git status --porcelain=v1 --untracked-files=all --ignore-submodules=none)"
  [[ -z "$status" ]] || { printf '%s\n' "$status" >&2; die "Git 工作树不是干净状态"; }
}

assert_clean
gh auth status --hostname github.com >/dev/null 2>&1 \
  || die "gh 未登录 github.com，先执行 gh auth login"
gh repo view --json nameWithOwner --jq .nameWithOwner >/dev/null \
  || die "gh 无法访问当前 GitHub 仓库"

for workflow in "$WORKFLOW_IMAGE" "$WORKFLOW_IMAGE_DEPLOY" "$WORKFLOW_CONTENT"; do
  gh workflow view "$workflow" >/dev/null \
    || die "无法访问 workflow: $workflow"
done

git fetch --quiet origin "$BRANCH" || die "无法 fetch origin/$BRANCH"
git rev-parse --verify "refs/remotes/origin/$BRANCH" >/dev/null 2>&1 \
  || die "远端不存在 origin/$BRANCH"

head_sha="$(git rev-parse HEAD)"
if ! git merge-base --is-ancestor "refs/remotes/origin/$BRANCH" HEAD; then
  die "origin/$BRANCH 已领先当前 HEAD；请先同步远端后重新部署"
fi
[[ "$head_sha" != "$(git rev-parse "refs/remotes/origin/$BRANCH")" ]] \
  || die "当前 HEAD 已经推送到 origin/$BRANCH，没有新的 commit 可部署"

mapfile -t changed_files < <(git diff --name-only "refs/remotes/origin/$BRANCH...HEAD")
image_expected=false
content_expected=false
for file in "${changed_files[@]}"; do
  case "$file" in
    book/*|docs/*)
      content_expected=true
      ;;
  esac
  case "$file" in
    package.json|package-lock.json|tsconfig.base.json|Dockerfile|docker-compose.yml|docker-compose*.yml|build_all_dot_file.py|bin/*|src/online_judge/*|packages/*|site/theme/*|site/public/*|site/markdown-style/*|site/widgets/*|scripts/deploy-vps.sh|deploy.sh|.github/workflows/*)
      image_expected=true
      ;;
  esac
done

[[ "$image_expected" == true || "$content_expected" == true ]] \
  || die "当前 commit 没有匹配任何部署路径"

runtime_dir="$(mktemp -d "${TMPDIR:-/tmp}/rbook-deploy.XXXXXX")"
trap 'rm -rf "$runtime_dir"' EXIT
export RBOOK_CONTENT_DIR="$ROOT_DIR/book"
export RBOOK_CODE_DIR="$ROOT_DIR/book/code"
export RBOOK_RUNTIME_DIR="$runtime_dir"
export RBOOK_TEST_STATIC_DIR="$runtime_dir/dist"

echo "[deploy] 本地验证 commit $head_sha"
npm run typecheck
npm run build:runtime
npm run test:api

assert_clean
[[ "$(git rev-parse HEAD)" == "$head_sha" ]] || die "本地验证改变了 HEAD"

echo "[deploy] 推送 $head_sha 到 origin/$BRANCH"
git push --no-verify origin "HEAD:$BRANCH"

declare -A run_ids=()
declare -A run_urls=()
declare -A run_statuses=()
declare -A run_conclusions=()

expected_workflows=()
[[ "$image_expected" == true ]] && expected_workflows+=("$WORKFLOW_IMAGE" "$WORKFLOW_IMAGE_DEPLOY")
[[ "$content_expected" == true ]] && expected_workflows+=("$WORKFLOW_CONTENT")

workflow_present() {
  local wanted="$1" workflow
  for workflow in "${expected_workflows[@]}"; do
    [[ "$workflow" == "$wanted" ]] && return 0
  done
  return 1
}

refresh_runs() {
  local record workflow id url status conclusion
  while IFS=$'\t' read -r workflow id url status conclusion; do
    [[ -n "$workflow" && -n "$id" ]] || continue
    workflow_present "$workflow" || continue
    run_ids["$workflow"]="$id"
    run_urls["$workflow"]="$url"
    run_statuses["$workflow"]="$status"
    run_conclusions["$workflow"]="$conclusion"
  done < <(gh run list --commit "$head_sha" --limit 100 \
    --json workflowName,databaseId,url,status,conclusion \
    --jq '.[] | [.workflowName, (.databaseId|tostring), .url, .status, (.conclusion // "")] | @tsv' 2>/dev/null || true)
}

discovery_deadline=$((SECONDS + DISCOVERY_TIMEOUT))
while (( SECONDS < discovery_deadline )); do
  refresh_runs
  missing=false
  for workflow in "${expected_workflows[@]}"; do
    [[ -n "${run_ids[$workflow]:-}" ]] || missing=true
  done
  [[ "$missing" == false ]] && break
  sleep 3
done

refresh_runs
for workflow in "${expected_workflows[@]}"; do
  [[ -n "${run_ids[$workflow]:-}" ]] \
    || die "已推送 $head_sha，但在 ${DISCOVERY_TIMEOUT}s 内没有找到 workflow: $workflow"
done

for workflow in "${expected_workflows[@]}"; do
  echo "[deploy] 监控 $workflow: ${run_urls[$workflow]}"
  set +e
  timeout --foreground "$WAIT_TIMEOUT" gh run watch "${run_ids[$workflow]}" --exit-status
  watch_status=$?
  set -e
  refresh_runs
  if [[ "$watch_status" == 124 ]]; then
    die "$workflow 等待超时（${WAIT_TIMEOUT}s）"
  fi
  if (( watch_status != 0 )); then
    echo "[deploy] $workflow 失败: status=${run_statuses[$workflow]} conclusion=${run_conclusions[$workflow]}" >&2
    exit "$watch_status"
  fi
done

echo "[deploy] 部署成功: $head_sha"
