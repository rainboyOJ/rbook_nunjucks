#!/usr/bin/env bash

# 获取脚本所在目录
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# 计算项目根目录 (向上两级)
PROJECT_DIR="$(dirname "$(dirname "$SCRIPT_DIR")")"
# 计算 index.js 的相对路径
INDEX_JS_PATH="$PROJECT_DIR/src/online_judge/index.js"

# 使用 node 调用 index.js
node "$INDEX_JS_PATH" $@