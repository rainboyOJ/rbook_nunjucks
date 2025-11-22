#!/usr/bin/env bash

# 删除dist目录
rm -rf dist

# 运行rbook build命令
node bin/rbook.js build


# 编译markdown样式
npx sass --load-path=src/rbook/markdown-it/assets markdown-style/markdown.scss dist/markdown.css

## 把 book/目录下的 所有的 图片文件(png,jpg,svg )等等, 按照原路径复制到 dist/ 目录下
# 使用 find 命令查找所有图片文件并复制到 dist 目录，保持目录结构
mkdir -p dist
find book/ -type f \( -iname "*.png" -o -iname "*.jpg" -o -iname "*.jpeg" -o -iname "*.gif" -o -iname "*.svg" -o -iname "*.ico" -o -iname "*.webp" \) | while read file; do
  # 计算相对于 book/ 目录的路径
  relative_path=${file#book/}
  # 创建目标目录
  mkdir -p "dist/$(dirname "$relative_path")"
  # 复制文件
  cp "$file" "dist/$relative_path"
done


# # 编译代码模板过滤器
npx vite build --base code_template \
  --outDir ../../dist/code_template  \
  --emptyOutDir ./third_part/code_template_filter
