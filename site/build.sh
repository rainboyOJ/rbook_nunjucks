#!/usr/bin/env bash
set -e

VITE_RBOOK_WEB_URL="https://rbook2.roj.ac.cn/"
REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
export RBOOK_APP_DIR="${RBOOK_APP_DIR:-site}"
export RBOOK_CONTENT_DIR="${RBOOK_CONTENT_DIR:-book}"
if [[ "$RBOOK_APP_DIR" = /* ]]; then
  APP_DIR="$RBOOK_APP_DIR"
else
  APP_DIR="$REPO_ROOT/$RBOOK_APP_DIR"
fi
if [[ "$RBOOK_CONTENT_DIR" = /* ]]; then
  CONTENT_DIR="$RBOOK_CONTENT_DIR"
else
  CONTENT_DIR="$REPO_ROOT/$RBOOK_CONTENT_DIR"
fi

cd "$REPO_ROOT"

# 删除dist目录
rm -rf "$APP_DIR/dist"

# 运行rbook build命令
node bin/rbook.js build || (echo "=======> 构建rbook build失败,检查错误" && exit 1 )



# 编译markdown样式
npx sass --load-path=packages/rbook-markdown/src/markdown-it/assets "$APP_DIR/markdown-style/markdown.scss" "$APP_DIR/dist/markdown.css"

## 把 pages/目录下的 所有的 图片文件(png,jpg,svg )等等, 按照原路径复制到 dist/ 目录下
# 使用 find 命令查找所有图片文件并复制到 dist 目录，保持目录结构
python3 build_all_dot_file.py "$CONTENT_DIR/pages" || echo "Warning: dot file build failed; continuing"
mkdir -p "$APP_DIR/dist"

# 关系图使用本地 D3，部署产物不依赖 CDN。
mkdir -p "$APP_DIR/dist/assets/vendor"
cp "$REPO_ROOT/node_modules/d3/dist/d3.min.js" "$APP_DIR/dist/assets/vendor/d3.min.js"

find "$CONTENT_DIR/pages/" -type f \( -iname "*.png" -o -iname "*.jpg" -o -iname "*.jpeg" -o -iname "*.gif" -o -iname "*.svg" -o -iname "*.ico" -o -iname "*.webp" \) | while read file; do
  # 计算相对于 pages/ 目录的路径
  relative_path=${file#"$CONTENT_DIR/pages/"}
  # 创建目标目录
  mkdir -p "$APP_DIR/dist/$(dirname "$relative_path")"
  # 复制文件
  cp "$file" "$APP_DIR/dist/$relative_path"
done

# 复制动画单页html
cp -r "$APP_DIR/widgets/animate_single_html" "$APP_DIR/dist/animate_single_html"

# 复制单文件 widgets
echo "=== 编译 rbook widgets ==="
declare -A RBOOK_WIDGETS=(
  [code_template_filter]=code_template
  [explore]=explore
  [article_inspector]=article_inspector
  [tags]=tags
  [relations]=relations
  [practice]=practice
  [diagnostics]=diagnostics
)
for source in "${!RBOOK_WIDGETS[@]}"; do
  target="${RBOOK_WIDGETS[$source]}"
  mkdir -p "$APP_DIR/dist/$target"
  cp "$APP_DIR/widgets/$source/index.html" "$APP_DIR/dist/$target/index.html"
done
