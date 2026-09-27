#!/usr/bin/env bash
set -euo pipefail

BASE_DIR="${RBOOK_BASE_DIR:-/opt/rbook}"
RELEASE_SHA="${RELEASE_SHA:?missing RELEASE_SHA}"
LOCK_HASH="${LOCK_HASH:?missing LOCK_HASH}"
DEPLOY_MODE="${DEPLOY_MODE:?missing DEPLOY_MODE}"
INCOMING_DIR="${INCOMING_DIR:?missing INCOMING_DIR}"
DEPLOY_ACTOR="${DEPLOY_ACTOR:-unknown}"
DEPLOY_SOURCE_HOST="${DEPLOY_SOURCE_HOST:-unknown}"
PUBLIC_HEALTH_URL="${PUBLIC_HEALTH_URL:-https://rbook2.roj.ac.cn/api/health}"

[[ "$RELEASE_SHA" =~ ^[0-9a-f]{40}$ ]] || { echo "invalid RELEASE_SHA" >&2; exit 2; }
[[ "$LOCK_HASH" =~ ^[0-9a-f]{64}$ ]] || { echo "invalid LOCK_HASH" >&2; exit 2; }
[[ "$DEPLOY_MODE" == content || "$DEPLOY_MODE" == application ]] \
  || { echo "invalid DEPLOY_MODE" >&2; exit 2; }
[[ "$(id -u)" == 0 ]] || { echo "deploy-native.sh must run as root" >&2; exit 2; }

RELEASES_DIR="$BASE_DIR/releases"
DEPENDENCIES_DIR="$BASE_DIR/dependencies"
CURRENT_LINK="$BASE_DIR/current"
CONFIG_FILE="$BASE_DIR/deploy.env"
AUDIT_LOG="$BASE_DIR/deployments.log"
BIN_DIR="$BASE_DIR/bin"
LOCK_FILE="$BASE_DIR/.deploy.lock"
RELEASE_DIR="$RELEASES_DIR/$RELEASE_SHA"
DEPENDENCY_DIR="$DEPENDENCIES_DIR/$LOCK_HASH"
SERVICE_FILE="$INCOMING_DIR/rbook.service"
RELEASE_ARCHIVE="$INCOMING_DIR/rbook-release-${RELEASE_SHA:0:12}.tar.zst"
DEPENDENCY_ARCHIVE="$INCOMING_DIR/rbook-dependencies-${LOCK_HASH:0:16}.tar.zst"
STARTED_AT="$(date --iso-8601=seconds)"
RESULT="failed"
DETAIL="unexpected-error"
CANDIDATE_PID=""
OLD_RELEASE=""
LEGACY_CONTAINER_RUNNING=false
PUBLIC_WAS_HEALTHY=false

mkdir -p "$BASE_DIR"
touch "$AUDIT_LOG"
chmod 600 "$AUDIT_LOG"
exec 9>"$LOCK_FILE"
flock -x -w 1800 9 || { echo "timed out waiting for $LOCK_FILE" >&2; exit 1; }

audit() {
  local ended_at
  ended_at="$(date --iso-8601=seconds)"
  printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' \
    "$STARTED_AT" "$ended_at" "$RELEASE_SHA" "$DEPLOY_MODE" \
    "$DEPLOY_ACTOR" "$DEPLOY_SOURCE_HOST" "$RESULT" "$DETAIL" >> "$AUDIT_LOG"
}

cleanup() {
  if [[ -n "$CANDIDATE_PID" ]]; then
    kill "$CANDIDATE_PID" >/dev/null 2>&1 || true
    wait "$CANDIDATE_PID" >/dev/null 2>&1 || true
  fi
  audit
}
trap cleanup EXIT

health_ok() {
  local url="$1"
  local attempts="${2:-30}"
  local response
  for _attempt in $(seq 1 "$attempts"); do
    if response="$(curl -fsS --max-time 5 "$url" 2>/dev/null)"; then
      if printf '%s' "$response" | node -e '
        let body = "";
        process.stdin.on("data", (chunk) => body += chunk);
        process.stdin.on("end", () => {
          const health = JSON.parse(body);
          if (health.ok !== true || health.stats?.errors !== 0) process.exit(1);
        });
      ' >/dev/null 2>&1; then
        return 0
      fi
    fi
    sleep 2
  done
  return 1
}

ensure_config() {
  if [[ -f "$CONFIG_FILE" ]]; then
    chmod 600 "$CONFIG_FILE"
    return
  fi

  local admin_token=""
  if docker inspect rbook >/dev/null 2>&1; then
    admin_token="$(docker inspect --format '{{range .Config.Env}}{{println .}}{{end}}' rbook \
      | sed -n 's/^RBOOK_ADMIN_TOKEN=//p' | head -1)"
  fi

  local previous_umask
  previous_umask="$(umask)"
  umask 077
  {
    printf 'HOST=0.0.0.0\n'
    printf 'PORT=3000\n'
    printf 'RBOOK_ADMIN_TOKEN=%q\n' "$admin_token"
    printf 'PCS2_API_BASE_URL=http://127.0.0.1:3300\n'
    printf 'PCS2_PUBLIC_BASE_URL=https://pcs2.roj.ac.cn\n'
  } > "$CONFIG_FILE"
  umask "$previous_umask"
}

assemble_release() {
  mkdir -p "$RELEASES_DIR" "$DEPENDENCIES_DIR" "$BIN_DIR"
  chmod 755 "$BASE_DIR" "$RELEASES_DIR" "$DEPENDENCIES_DIR" "$BIN_DIR"

  if [[ ! -d "$DEPENDENCY_DIR/node_modules" ]]; then
    [[ -f "$DEPENDENCY_ARCHIVE" ]] || {
      echo "missing dependency archive $DEPENDENCY_ARCHIVE" >&2
      return 1
    }
    (cd "$INCOMING_DIR" && sha256sum -c "$(basename "$DEPENDENCY_ARCHIVE").sha256")
    local dependency_tmp="$DEPENDENCIES_DIR/.new-$LOCK_HASH"
    rm -rf "$dependency_tmp"
    mkdir -p "$dependency_tmp"
    zstd -dc "$DEPENDENCY_ARCHIVE" | tar -xf - -C "$dependency_tmp"
    mv "$dependency_tmp" "$DEPENDENCY_DIR"
  fi

  (cd "$INCOMING_DIR" && sha256sum -c "$(basename "$RELEASE_ARCHIVE").sha256")
  local release_tmp="$RELEASES_DIR/.new-$RELEASE_SHA"
  rm -rf "$release_tmp"
  mkdir -p "$release_tmp"
  zstd -dc "$RELEASE_ARCHIVE" | tar -xf - -C "$release_tmp"
  grep -qx "RBOOK_RELEASE_SHA=$RELEASE_SHA" "$release_tmp/release.env"
  grep -qx "RBOOK_DEPENDENCY_HASH=$LOCK_HASH" "$release_tmp/release.env"

  cp -al "$DEPENDENCY_DIR/node_modules" "$release_tmp/node_modules"
  rm -rf "$release_tmp/node_modules/@rbook"
  mkdir -p "$release_tmp/node_modules/@rbook"
  for workspace_name in rbook-cli rbook-core rbook-markdown rbook-search rbook-server; do
    package_name="${workspace_name#rbook-}"
    ln -s "../../packages/$workspace_name" "$release_tmp/node_modules/@rbook/$package_name"
  done

  rm -rf "$RELEASE_DIR"
  mv "$release_tmp" "$RELEASE_DIR"
  chown -R root:rbook "$RELEASE_DIR" "$DEPENDENCY_DIR"
  chmod -R a=rX,u+w "$RELEASE_DIR" "$DEPENDENCY_DIR"
  chown -R rbook:rbook "$RELEASE_DIR/runtime/.search"
  chmod -R u+rwX "$RELEASE_DIR/runtime/.search"
}

start_candidate() {
  local candidate_port
  candidate_port="$(python3 - <<'PY'
import socket
with socket.socket() as sock:
    sock.bind(('127.0.0.1', 0))
    print(sock.getsockname()[1])
PY
)"

  set -a
  # shellcheck disable=SC1090
  source "$CONFIG_FILE"
  set +a

  runuser -u rbook --preserve-environment -- \
    env \
      NODE_ENV=production \
      HOST=127.0.0.1 \
      PORT="$candidate_port" \
      RBOOK_CONTENT_DIR="$RELEASE_DIR/book" \
      RBOOK_CODE_DIR="$RELEASE_DIR/book/code" \
      RBOOK_RUNTIME_DIR="$RELEASE_DIR/runtime" \
      RBOOK_DOCS_DIR="$RELEASE_DIR/docs" \
      node "$RELEASE_DIR/packages/rbook-server/dist/serve.js" \
    >"$INCOMING_DIR/candidate.log" 2>&1 &
  CANDIDATE_PID=$!

  if ! health_ok "http://127.0.0.1:$candidate_port/api/health" 30; then
    cat "$INCOMING_DIR/candidate.log" >&2 || true
    return 1
  fi

  kill "$CANDIDATE_PID" >/dev/null 2>&1 || true
  wait "$CANDIDATE_PID" >/dev/null 2>&1 || true
  CANDIDATE_PID=""
}

point_current_at() {
  local target="$1"
  local next_link="$BASE_DIR/.current-${RELEASE_SHA}"
  rm -f "$next_link"
  ln -s "$target" "$next_link"
  mv -Tf "$next_link" "$CURRENT_LINK"
}

rollback() {
  echo "[native-deploy] rolling back" >&2
  systemctl stop rbook.service >/dev/null 2>&1 || true

  if [[ -n "$OLD_RELEASE" && -d "$OLD_RELEASE" ]]; then
    point_current_at "$OLD_RELEASE"
    systemctl restart rbook.service
    health_ok http://127.0.0.1:3000/api/health 30 || true
    DETAIL="rolled-back-to-$(basename "$OLD_RELEASE")"
    return
  fi

  if [[ "$LEGACY_CONTAINER_RUNNING" == true ]]; then
    docker update --restart=unless-stopped rbook >/dev/null 2>&1 || true
    docker start rbook >/dev/null
    health_ok http://127.0.0.1:3000/api/health 30 || true
    DETAIL="rolled-back-to-docker"
    return
  fi

  DETAIL="rollback-unavailable"
}

prune_old_releases() {
  local keep_one="$RELEASE_DIR"
  local keep_two="$OLD_RELEASE"
  local candidate
  while IFS= read -r candidate; do
    [[ "$candidate" == "$keep_one" || "$candidate" == "$keep_two" ]] && continue
    rm -rf "$candidate"
  done < <(find "$RELEASES_DIR" -mindepth 1 -maxdepth 1 -type d ! -name '.new-*' -print)

  local dependency_hash dependency_path used
  while IFS= read -r dependency_path; do
    dependency_hash="$(basename "$dependency_path")"
    used=false
    while IFS= read -r candidate; do
      if grep -qx "RBOOK_DEPENDENCY_HASH=$dependency_hash" "$candidate/release.env" 2>/dev/null; then
        used=true
        break
      fi
    done < <(find "$RELEASES_DIR" -mindepth 1 -maxdepth 1 -type d -print)
    [[ "$used" == true ]] || rm -rf "$dependency_path"
  done < <(find "$DEPENDENCIES_DIR" -mindepth 1 -maxdepth 1 -type d ! -name '.new-*' -print)
}

for command_name in node curl zstd tar sha256sum systemctl runuser python3 flock getent groupadd useradd usermod; do
  command -v "$command_name" >/dev/null 2>&1 || {
    echo "missing command: $command_name" >&2
    exit 1
  }
done

if health_ok "$PUBLIC_HEALTH_URL" 2; then
  PUBLIC_WAS_HEALTHY=true
fi

if ! getent group rbook >/dev/null 2>&1; then
  groupadd --system rbook
fi
if ! id rbook >/dev/null 2>&1; then
  useradd --system --gid rbook --home-dir "$BASE_DIR" --shell /usr/sbin/nologin rbook
else
  # An older Docker deployment may have left a login-capable account behind.
  # Keep the service identity deliberately unprivileged and out of Docker's
  # effectively-root supplementary group.
  usermod --home "$BASE_DIR" --shell /usr/sbin/nologin --gid rbook --groups '' --lock rbook
fi

ensure_config
assemble_release

install -m 0644 "$SERVICE_FILE" /etc/systemd/system/rbook.service
install -m 0755 "$INCOMING_DIR/deploy-native.sh" "$BIN_DIR/deploy-native.sh"
systemctl daemon-reload
systemctl enable rbook.service >/dev/null

if [[ -L "$CURRENT_LINK" ]]; then
  OLD_RELEASE="$(readlink -f "$CURRENT_LINK")"
fi
if docker inspect rbook >/dev/null 2>&1; then
  docker inspect rbook > "$BASE_DIR/legacy-rbook-container.json"
  if [[ "$(docker inspect --format '{{.State.Running}}' rbook)" == true ]]; then
    LEGACY_CONTAINER_RUNNING=true
  fi
fi

echo "[native-deploy] candidate check"
start_candidate

if [[ "$LEGACY_CONTAINER_RUNNING" == true ]]; then
  docker update --restart=no rbook >/dev/null
  docker stop -t 20 rbook >/dev/null
else
  systemctl stop rbook.service >/dev/null 2>&1 || true
fi

point_current_at "$RELEASE_DIR"
systemctl restart rbook.service

if ! health_ok http://127.0.0.1:3000/api/health 30; then
  journalctl -u rbook.service -n 120 --no-pager >&2 || true
  rollback
  exit 1
fi

if [[ "$PUBLIC_WAS_HEALTHY" == true ]] && ! health_ok "$PUBLIC_HEALTH_URL" 15; then
  echo "[native-deploy] public health check failed" >&2
  rollback
  exit 1
fi

if [[ "$PUBLIC_WAS_HEALTHY" != true ]]; then
  echo "[native-deploy] public endpoint was already unhealthy before deployment; local health check is authoritative" >&2
fi

if docker inspect rbook >/dev/null 2>&1; then
  docker rm rbook >/dev/null
fi

prune_old_releases
rm -rf "$INCOMING_DIR"
RESULT="success"
DETAIL="health-checks-passed"
echo "[native-deploy] deployed $RELEASE_SHA"
