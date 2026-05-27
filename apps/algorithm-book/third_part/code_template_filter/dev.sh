#!/usr/bin/env bash
set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
npx vite dev --host --config "$SCRIPT_DIR/vite.config.js"
