#!/usr/bin/env bash
set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
npx vite build --config "$SCRIPT_DIR/vite.config.ts" --base /code_template/
