#!/usr/bin/env bash
# 对 book/code 下自包含的 C++ 模板做语法检查（g++ -fsyntax-only）。
#
# 判断标准：文件前 40 行里出现过 #include，说明它自己带齐了头文件，可以单独编译。
# 片段式模板（依赖外部 maxn / e / head 等符号）故意跳过，它们不是完整翻译单元。
#
# 用法：bash scripts/check-cpp-templates.sh [代码目录]
set -euo pipefail

ROOT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)"
CODE_DIR="${1:-$ROOT_DIR/book/code}"

# 已知不能直接编译的文件，原因写清楚，修好后从这里删掉
SKIP_FILES=(
  # 空函数体、少分号的未完成草稿（rotateLeft/rotateRight/rank 都还没写）
  "data-struture/bst_command2.cpp"
)

# template/ 下是带 {{...}} 站点指令的模板源，不是合法 C++
SKIP_PREFIXES=(
  "template/"
)

is_skipped() {
    local rel="$1" item
    for item in "${SKIP_FILES[@]}"; do
        [[ "$rel" == "$item" ]] && return 0
    done
    for item in "${SKIP_PREFIXES[@]}"; do
        [[ "$rel" == "$item"* ]] && return 0
    done
    return 1
}

if ! command -v g++ >/dev/null 2>&1; then
    echo "[cpp-check] 没找到 g++，跳过检查"
    exit 0
fi

checked=0
fragment=0
skipped=0
failed=0

while IFS= read -r -d '' file; do
    rel="${file#"$CODE_DIR"/}"
    if ! head -n 40 "$file" | grep -q '^[[:space:]]*#include'; then
        fragment=$((fragment + 1))
        continue
    fi
    if is_skipped "$rel"; then
        skipped=$((skipped + 1))
        continue
    fi
    checked=$((checked + 1))
    if ! output="$(g++ -std=c++17 -fsyntax-only "$file" 2>&1)"; then
        printf '\n[cpp-check] 编译失败: %s\n%s\n' "$rel" "$output"
        failed=$((failed + 1))
    fi
done < <(find "$CODE_DIR" -name '*.cpp' -print0 | sort -z)

echo "[cpp-check] checked=$checked fragment=$fragment skipped=$skipped failed=$failed"
if (( failed > 0 )); then
    echo "[cpp-check] 上面这些模板单独编译不过，说明少了头文件或者写法有问题" >&2
    exit 1
fi
