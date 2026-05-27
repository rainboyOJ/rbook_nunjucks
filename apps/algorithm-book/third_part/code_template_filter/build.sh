#!/usr/bin/env bash
set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
npx vite build --config "$SCRIPT_DIR/vite.config.js" --base /code_template/
