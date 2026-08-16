# RBook SVG 图示规范

## 目标

RBook 图示采用克制的教材几何风格。图形负责表达空间关系，正文负责解释概念和推导。保持准确、安静、易扫描，不追求插画感。

## 格式边界

使用手写 SVG：

- 固定的局部树形或图结构；
- 分块、切边、集合包含；
- 数组、区间、指针；
- 前后状态或步骤迁移。

保留 DOT：

- 大型树或网络；
- 节点和边经常变化；
- 依赖自动布局或程序批量生成。

手写 `.svg` 是唯一源文件，不附带 Draw.io、Excalidraw、D2 或位图源文件。

## 设计令牌

| 用途 | 值 |
|---|---|
| 画布 | `#ffffff` |
| 主文字 | `#1f2937` |
| 次要文字 | `#64748b` |
| 普通边 | `#94a3b8`，`2px` |
| 强调边 | `3px` |
| 主色 | `#2563eb` |
| 正向、减少 | `#059669` |
| 警示、增加 | `#d97706` |
| 错误、矛盾 | `#dc2626` |
| 蓝色浅填充 | `#eff6ff` |
| 绿色浅填充 | `#ecfdf5` |
| 橙色浅填充 | `#fff7ed` |
| 红色浅填充 | `#fef2f2` |
| 普通节点 | 直径 `44px` |
| 重点节点 | 直径 `52px` |
| 标签 | `16px / 600` |
| 次要文字 | `14px / 400`，不得更小 |
| 圆角 | `8px` |
| 外边距 | 至少 `24px` |

字体使用：

```css
font-family: system-ui, -apple-system, "Segoe UI", "Noto Sans CJK SC",
  "Microsoft YaHei", sans-serif;
```

默认值应保持统一。只有图的语义确实需要时才局部调整，并保持同一张图内一致。

## 画布与响应式

- SVG 同时提供 `viewBox` 及与其匹配的数值型 `width`、`height`。
- 默认 `viewBox` 宽度为 560 到 720。
- 使用完整白色 `.canvas` 矩形作为背景。外链 `<img>` 无法读取页面的 `data-darkmode`，固定浅色画布比错误的主题适配更可靠。
- 图过宽时改为纵向布局或拆图，不把正文缩成小字。
- 在 720px 与 360px 两个宽度实际渲染。360px 下关键标签应接近 12px，并保持可辨认。
- 保留稳定宽高比，避免图片加载时页面跳动。

## 图中文字

- 只写图形本身无法替代的标签。
- 不在图中重复文章标题、公式推导和结论。
- 单个中文标签尽量不超过 8 个汉字。
- 复杂数学留在 Markdown。SVG 只使用 `→`、`≤`、`−`、`n − s` 等短表达。
- 上下标可用 `<tspan baseline-shift="sub">`，不要使用 MathJax、KaTeX、HTML 或文字转路径。
- 不使用负字距或随视口缩放的字号。

## 几何与层级

- 普通节点使用 44px 圆；重点节点使用 52px 圆和 3px 描边。
- 普通边用 2px 中性色；方向或当前动作才使用 3px 主色箭头。
- 分区使用浅填充和 2px 边界。虚线表示逻辑分组或省略，不作为装饰。
- 圆角矩形统一使用 8px 圆角。
- 元素优先对齐到整数坐标，避免无意义的小数精度。
- 每张图只设置一个主要视觉焦点。

禁用：

- 渐变、阴影、发光、纹理；
- 装饰性背景或图案；
- 纯靠颜色区分状态；
- 大段说明框；
- 无意义的外层卡片和多层套框。

## 语义色

颜色含义保持稳定：

- 蓝色：当前点、选中状态、主要方向；
- 绿色：减少、成立、目标侧；
- 橙色：增加、待处理、另一侧；
- 红色：错误、冲突、反例。

颜色不是唯一编码。配合文字、箭头、边框或线型，让灰度环境下仍可理解。

## 源码结构

SVG 根元素应包含：

```xml
<svg
  xmlns="http://www.w3.org/2000/svg"
  width="640"
  height="320"
  viewBox="0 0 640 320"
  role="img"
  aria-labelledby="diagram-title diagram-desc"
>
  <title id="diagram-title">简短标题</title>
  <desc id="diagram-desc">说明图中关系，而不是重复标题。</desc>
  <style>...</style>
  <rect class="canvas" width="640" height="320" />
  <g id="...">...</g>
</svg>
```

- 在一个 `<style>` 中定义令牌和语义 class。
- 使用 `<g id="...">` 按区域、状态或步骤分组。
- `<defs>` 只存放实际使用的箭头、裁剪等资源。
- 删除编辑器元数据、隐藏图层和无意义注释。
- 禁止 `<script>`、`foreignObject`、事件属性、远程资源、外部字体和嵌入位图。

## 可访问性

- Markdown 必须提供有意义的 `alt`。
- SVG 必须包含非空 `<title>` 和 `<desc>`。
- 根元素使用 `role="img"`，`aria-labelledby` 同时引用标题和描述。
- 文字保留为 `<text>`，不转路径。
- 标签与背景保持清晰对比。
- 红绿状态必须额外使用文字、形状、箭头或线型区分。

## 文件约定

新图放在文章本地：

```text
book/pages/<article>/images/<descriptive-name>.svg
```

文件名使用英文 `kebab-case`。Markdown 使用站点根绝对路径，例如 `book/pages/tree-algo/example/images/state.svg` 写成 `/tree-algo/example/images/state.svg`。不要使用 `./images/...`，因为无尾斜杠的文章 URL 会把它解析到上一级目录；也不为统一格式批量移动旧图。

## 验收命令

```bash
node .agents/skills/rbook-svg-diagram/scripts/validate-svg.mjs <svg-path>
node .agents/skills/rbook-svg-diagram/scripts/render-svg.mjs <svg-path>
```

校验通过后仍需查看两个 PNG。自动检查不能判断信息层级、遮挡和教学表达是否清楚。
