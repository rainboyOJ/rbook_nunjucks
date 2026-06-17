#!/usr/bin/env bash
set -euo pipefail

# VPS 端通用部署脚本。
# DEPLOY_MODE=image：部署指定的新 Docker 镜像。
# DEPLOY_MODE=content：只更新文章内容，复用当前容器镜像，缺失时使用 FALLBACK_IMAGE。

DEPLOY_MODE="${DEPLOY_MODE:-image}"
CONTAINER_NAME="${CONTAINER_NAME:?missing CONTAINER_NAME}"
HOST_PORT="${HOST_PORT:?missing HOST_PORT}"
CONTAINER_PORT="${CONTAINER_PORT:?missing CONTAINER_PORT}"
RBOOK_ADMIN_TOKEN="${RBOOK_ADMIN_TOKEN:-}"
VPS_REPO_DIR="${VPS_REPO_DIR:?missing VPS_REPO_DIR}"
VPS_REPO_BRANCH="${VPS_REPO_BRANCH:?missing VPS_REPO_BRANCH}"
VPS_REPO_URL="${VPS_REPO_URL:?missing VPS_REPO_URL}"

init_content_repo() {
  if [ ! -d "$VPS_REPO_DIR/.git" ]; then
    echo "Initializing sparse content repo in $VPS_REPO_DIR"
    rm -rf "$VPS_REPO_DIR"
    mkdir -p "$VPS_REPO_DIR"
    git -C "$VPS_REPO_DIR" init
    git -C "$VPS_REPO_DIR" remote add origin "$VPS_REPO_URL"
  fi

  git -C "$VPS_REPO_DIR" remote set-url origin "$VPS_REPO_URL"
  git -C "$VPS_REPO_DIR" sparse-checkout init --cone
  git -C "$VPS_REPO_DIR" sparse-checkout set book
  git -C "$VPS_REPO_DIR" fetch --depth=1 origin "$VPS_REPO_BRANCH"
  git -C "$VPS_REPO_DIR" reset --hard "origin/$VPS_REPO_BRANCH"
}

select_image() {
  case "$DEPLOY_MODE" in
    image)
      IMAGE="${IMAGE:?missing IMAGE when DEPLOY_MODE=image}"
      echo "Pulling $IMAGE"
      timeout 10m docker pull --quiet "$IMAGE"
      echo "Pulled $IMAGE"
      ;;
    content)
      IMAGE="$(docker inspect --format '{{.Config.Image}}' "$CONTAINER_NAME" 2>/dev/null || true)"
      if [ -z "$IMAGE" ]; then
        IMAGE="${FALLBACK_IMAGE:?missing FALLBACK_IMAGE when current container image is unavailable}"
        echo "No running image found; pulling fallback $IMAGE"
        timeout 10m docker pull --quiet "$IMAGE"
      else
        echo "Reusing current image $IMAGE"
      fi
      ;;
    *)
      echo "Unknown DEPLOY_MODE: $DEPLOY_MODE" >&2
      return 2
      ;;
  esac
}

verify_runtime_build() {
  local test_name="${CONTAINER_NAME}-${DEPLOY_MODE}-test"

  docker rm -f "$test_name" >/dev/null 2>&1 || true
  docker run --rm \
    --name "$test_name" \
    -e NODE_ENV=production \
    -e RBOOK_CONTENT_DIR=/content \
    -e RBOOK_RUNTIME_DIR=/tmp/rbook-runtime \
    -v "$VPS_REPO_DIR/book:/content:ro" \
    "$IMAGE" \
    npm run build:runtime
}

restart_container() {
  docker rm -f "$CONTAINER_NAME" >/dev/null 2>&1 || true
  docker run -d \
    --name "$CONTAINER_NAME" \
    --restart unless-stopped \
    -p "${HOST_PORT}:${CONTAINER_PORT}" \
    -e NODE_ENV=production \
    -e HOST=0.0.0.0 \
    -e PORT="$CONTAINER_PORT" \
    -e RBOOK_ADMIN_TOKEN="$RBOOK_ADMIN_TOKEN" \
    -e RBOOK_CONTENT_DIR=/content \
    -e RBOOK_RUNTIME_DIR=/tmp/rbook-runtime \
    -v "$VPS_REPO_DIR/book:/content:ro" \
    "$IMAGE"

  if [ "$DEPLOY_MODE" = "image" ]; then
    docker image prune -f
  fi

  docker ps --filter "name=$CONTAINER_NAME"
}

wait_for_health() {
  local health_url="http://127.0.0.1:${HOST_PORT}/api/health"
  local response=""

  echo "Waiting for $health_url"
  for attempt in $(seq 1 60); do
    if response="$(curl -fsS --max-time 5 "$health_url" 2>/tmp/rbook-health-error.log)"; then
      if printf '%s' "$response" | docker exec -i "$CONTAINER_NAME" node -e '
        let body = "";
        process.stdin.on("data", (chunk) => {
          body += chunk;
        });
        process.stdin.on("end", () => {
          const health = JSON.parse(body);
          if (health.ok !== true) {
            throw new Error("health.ok is not true");
          }
          if (!health.stats || health.stats.errors !== 0) {
            throw new Error(`health.stats.errors is ${health.stats?.errors}`);
          }
        });
      ' >/dev/null 2>/tmp/rbook-health-parse-error.log; then
        echo "Health check passed"
        return 0
      fi
    fi

    echo "Health check attempt ${attempt}/60 failed; retrying in 5s"
    sleep 5
  done

  echo "Health check failed"
  echo "Last curl error:"
  cat /tmp/rbook-health-error.log 2>/dev/null || true
  echo "Last health response:"
  printf '%s\n' "$response"
  echo "Last parse error:"
  cat /tmp/rbook-health-parse-error.log 2>/dev/null || true
  echo "Container logs:"
  docker logs --tail 200 "$CONTAINER_NAME" || true
  return 1
}

init_content_repo
select_image
verify_runtime_build
restart_container
wait_for_health
