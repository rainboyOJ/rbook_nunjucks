#!/usr/bin/env bash
set -e

VITE_RBOOK_WEB_URL="https://rbook2.roj.ac.cn/"
REPO_ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
APP_DIR="$(cd "$(dirname "$0")" && pwd)"
export RBOOK_APP_DIR="${RBOOK_APP_DIR:-apps/algorithm-book}"

cd "$REPO_ROOT"

# 删除dist目录
rm -rf "$APP_DIR/dist"

# 运行rbook build命令
node bin/rbook.js build || (echo "=======> 构建rbook build失败,检查错误" && exit 1 )



# 编译markdown样式
npx sass --load-path=packages/rbook-markdown/src/markdown-it/assets "$APP_DIR/markdown-style/markdown.scss" "$APP_DIR/dist/markdown.css"

## 把 book/目录下的 所有的 图片文件(png,jpg,svg )等等, 按照原路径复制到 dist/ 目录下
# 使用 find 命令查找所有图片文件并复制到 dist 目录，保持目录结构
python3 build_all_dot_file.py "$APP_DIR/book" || echo "Warning: dot file build failed; continuing"
mkdir -p "$APP_DIR/dist"
find "$APP_DIR/book/" -type f \( -iname "*.png" -o -iname "*.jpg" -o -iname "*.jpeg" -o -iname "*.gif" -o -iname "*.svg" -o -iname "*.ico" -o -iname "*.webp" \) | while read file; do
  # 计算相对于 book/ 目录的路径
  relative_path=${file#"$APP_DIR/book/"}
  # 创建目标目录
  mkdir -p "$APP_DIR/dist/$(dirname "$relative_path")"
  # 复制文件
  cp "$file" "$APP_DIR/dist/$relative_path"
done

# 复制动画单页html
cp -r "$APP_DIR/third_part/animate_single_html" "$APP_DIR/dist/animate_single_html"

# # 编译代码模板过滤器
echo "=== 编译代码模板过滤器 ==="
npx vite build \
  --config "$APP_DIR/third_part/code_template_filter/vite.config.js" \
  --base /code_template/
