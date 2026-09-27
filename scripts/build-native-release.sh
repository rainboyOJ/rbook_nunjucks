#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)"
WORK_DIR=""
RELEASE_SHA=""
DEPLOY_MODE=""
CANDIDATE_PID=""

die() {
  echo "[release] $*" >&2
  exit 1
}

cleanup() {
  if [[ -n "$CANDIDATE_PID" ]]; then
    kill "$CANDIDATE_PID" >/dev/null 2>&1 || true
    wait "$CANDIDATE_PID" >/dev/null 2>&1 || true
  fi
}
trap cleanup EXIT

while (( $# > 0 )); do
  case "$1" in
    --work-dir)
      WORK_DIR="${2:?missing value for --work-dir}"
      shift 2
      ;;
    --commit)
      RELEASE_SHA="${2:?missing value for --commit}"
      shift 2
      ;;
    --mode)
      DEPLOY_MODE="${2:?missing value for --mode}"
      shift 2
      ;;
    *)
      die "unknown argument: $1"
      ;;
  esac
done

[[ -n "$WORK_DIR" ]] || die "--work-dir is required"
[[ "$RELEASE_SHA" =~ ^[0-9a-f]{40}$ ]] || die "--commit must be a full Git SHA"
[[ "$DEPLOY_MODE" == content || "$DEPLOY_MODE" == application ]] \
  || die "--mode must be content or application"

for command_name in npm node rsync tar zstd sha256sum curl python3; do
  command -v "$command_name" >/dev/null 2>&1 || die "missing command: $command_name"
done

mkdir -p "$WORK_DIR"
RUNTIME_DIR="$WORK_DIR/runtime-build"
RELEASE_DIR="$WORK_DIR/release"
RELEASE_ARCHIVE="$WORK_DIR/rbook-release-${RELEASE_SHA:0:12}.tar.zst"
LOCK_HASH="$(sha256sum "$ROOT_DIR/package-lock.json" | cut -d' ' -f1)"
DEPENDENCY_CACHE_ROOT="$ROOT_DIR/.git/rbook-native-dependencies"
DEPENDENCY_DIR="$DEPENDENCY_CACHE_ROOT/$LOCK_HASH"
DEPENDENCY_ARCHIVE="$WORK_DIR/rbook-dependencies-${LOCK_HASH:0:16}.tar.zst"

export RBOOK_CONTENT_DIR="$ROOT_DIR/book"
export RBOOK_CODE_DIR="$ROOT_DIR/book/code"
export RBOOK_RUNTIME_DIR="$RUNTIME_DIR"
export RBOOK_TEST_STATIC_DIR="$RUNTIME_DIR/dist"

echo "[release] compile and validate packages"
npm run build:packages

echo "[release] build runtime"
npm run build:runtime:compiled

if [[ "$DEPLOY_MODE" == application ]]; then
  echo "[release] run API tests"
  node --test \
    scripts/include-code.test.mjs \
    scripts/rbook-client.test.mjs \
    scripts/home-navigation.test.mjs
  node scripts/test-api-contract.mjs
fi

if [[ ! -d "$DEPENDENCY_DIR/node_modules" ]]; then
  echo "[release] build production dependencies for $LOCK_HASH"
  DEPENDENCY_BUILD_DIR="$WORK_DIR/dependency-build"
  mkdir -p "$DEPENDENCY_BUILD_DIR/packages"
  cp "$ROOT_DIR/package.json" "$ROOT_DIR/package-lock.json" "$DEPENDENCY_BUILD_DIR/"
  for package_dir in "$ROOT_DIR"/packages/*; do
    package_name="$(basename "$package_dir")"
    mkdir -p "$DEPENDENCY_BUILD_DIR/packages/$package_name"
    cp "$package_dir/package.json" "$DEPENDENCY_BUILD_DIR/packages/$package_name/"
  done
  (
    cd "$DEPENDENCY_BUILD_DIR"
    npm ci --omit=dev --legacy-peer-deps
  )
  mkdir -p "$DEPENDENCY_CACHE_ROOT"
  mv "$DEPENDENCY_BUILD_DIR" "$DEPENDENCY_DIR"
fi

echo "[release] stage release $RELEASE_SHA"
mkdir -p "$RELEASE_DIR"
cp "$ROOT_DIR/package.json" "$ROOT_DIR/package-lock.json" "$ROOT_DIR/build_all_dot_file.py" "$RELEASE_DIR/"
for source_dir in bin book docs packages site; do
  rsync -a \
    --exclude node_modules \
    --exclude .search \
    "$ROOT_DIR/$source_dir/" "$RELEASE_DIR/$source_dir/"
done
rsync -a "$RUNTIME_DIR/" "$RELEASE_DIR/runtime/"

cat > "$RELEASE_DIR/release.env" <<EOF
RBOOK_RELEASE_SHA=$RELEASE_SHA
RBOOK_DEPENDENCY_HASH=$LOCK_HASH
RBOOK_DEPLOY_MODE=$DEPLOY_MODE
EOF

echo "[release] test exact production dependency set"
cp -a "$DEPENDENCY_DIR/node_modules" "$RELEASE_DIR/node_modules"
rm -rf "$RELEASE_DIR/node_modules/@rbook"
mkdir -p "$RELEASE_DIR/node_modules/@rbook"
for workspace_name in rbook-cli rbook-core rbook-markdown rbook-search rbook-server; do
  package_name="${workspace_name#rbook-}"
  ln -s "../../packages/$workspace_name" "$RELEASE_DIR/node_modules/@rbook/$package_name"
done

CANDIDATE_PORT="$(python3 - <<'PY'
import socket
with socket.socket() as sock:
    sock.bind(('127.0.0.1', 0))
    print(sock.getsockname()[1])
PY
)"

env \
  NODE_ENV=production \
  HOST=127.0.0.1 \
  PORT="$CANDIDATE_PORT" \
  RBOOK_CONTENT_DIR="$RELEASE_DIR/book" \
  RBOOK_CODE_DIR="$RELEASE_DIR/book/code" \
  RBOOK_RUNTIME_DIR="$RELEASE_DIR/runtime" \
  RBOOK_DOCS_DIR="$RELEASE_DIR/docs" \
  PCS2_API_BASE_URL=http://127.0.0.1:3300 \
  node "$RELEASE_DIR/packages/rbook-server/dist/serve.js" \
  >"$WORK_DIR/candidate.log" 2>&1 &
CANDIDATE_PID=$!

candidate_ok=false
for _attempt in $(seq 1 30); do
  if curl -fsS --max-time 3 "http://127.0.0.1:$CANDIDATE_PORT/api/health" 2>/dev/null \
    | node -e '
      let body = "";
      process.stdin.on("data", (chunk) => body += chunk);
      process.stdin.on("end", () => {
        const health = JSON.parse(body);
        if (health.ok !== true || health.stats?.errors !== 0) process.exit(1);
      });
    ' >/dev/null 2>&1; then
    candidate_ok=true
    break
  fi
  sleep 1
done

if [[ "$candidate_ok" != true ]]; then
  cat "$WORK_DIR/candidate.log" >&2
  die "candidate release failed its local health check"
fi

kill "$CANDIDATE_PID" >/dev/null 2>&1 || true
wait "$CANDIDATE_PID" >/dev/null 2>&1 || true
CANDIDATE_PID=""
rm -rf "$RELEASE_DIR/node_modules"

echo "[release] compress release"
tar -C "$RELEASE_DIR" -cf - . | zstd -T0 -3 -q -o "$RELEASE_ARCHIVE"

if [[ ! -f "$DEPENDENCY_DIR/dependencies.tar.zst" ]]; then
  echo "[release] compress production dependencies"
  tar -C "$DEPENDENCY_DIR" -cf - node_modules \
    | zstd -T0 -3 -q -o "$DEPENDENCY_DIR/dependencies.tar.zst"
fi
cp "$DEPENDENCY_DIR/dependencies.tar.zst" "$DEPENDENCY_ARCHIVE"

(
  cd "$WORK_DIR"
  sha256sum "$(basename "$RELEASE_ARCHIVE")" > "$(basename "$RELEASE_ARCHIVE").sha256"
  sha256sum "$(basename "$DEPENDENCY_ARCHIVE")" > "$(basename "$DEPENDENCY_ARCHIVE").sha256"
)

{
  printf 'RELEASE_ARCHIVE=%q\n' "$RELEASE_ARCHIVE"
  printf 'DEPENDENCY_ARCHIVE=%q\n' "$DEPENDENCY_ARCHIVE"
  printf 'LOCK_HASH=%q\n' "$LOCK_HASH"
  printf 'RELEASE_SHA=%q\n' "$RELEASE_SHA"
} > "$WORK_DIR/artifacts.env"

echo "[release] release=$(du -h "$RELEASE_ARCHIVE" | cut -f1) dependencies=$(du -h "$DEPENDENCY_ARCHIVE" | cut -f1)"
