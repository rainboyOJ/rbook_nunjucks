---
name: rbook-svg-diagram
description: 在 rbook 算法电子书中创建、重画、迁移或优化手写 SVG 教学图时必须使用此 skill。用户提到 SVG 图示、算法插图、证明示意图、把 DOT 改成 SVG、统一图示风格，或文章写作过程中确实需要新增固定构图的教学图时都应触发。它负责选择 SVG/DOT、控制图中文字密度、应用 rbook 视觉规范、更新文章引用，并执行安全、可访问性、移动端和构建验收；大型自动布局拓扑仍使用 DOT。
compatibility: Requires Node.js, xmllint, and rsvg-convert; visual inspection uses the local image viewer.
---

# RBook SVG Diagram

为 rbook 创建可维护的教材型 SVG。目标不是把正文搬进图片，而是让空间关系、方向和状态变化一眼可见。

## 必读资源

开始编辑前：

1. 完整读取 `references/design-system.md`。
2. 根据图的结构读取一个最接近的模板：
   - 树或图：`assets/tree-graph.svg`
   - 分块、切边、集合包含：`assets/partition.svg`
   - 数组、区间、指针：`assets/sequence.svg`
   - 前后状态、步骤迁移：`assets/transition.svg`
   - 没有明显类型时从 `assets/base.svg` 开始。
3. 阅读目标文章中图片前后的正文，先确定正文已经解释了什么。

不要把模板原样交付。模板提供结构与样式写法，成品必须针对文章语义重新构图。

## 先决定是否使用 SVG

手写 SVG 适合固定、简单且教学意图明确的图，例如局部树形、切分关系、数组区间和状态迁移。

以下情况继续使用 DOT：

- 节点或边较多，需要自动布局；
- 图由程序批量生成；
- 核心价值是展示完整拓扑，而不是固定的视觉叙事。

用户只要求“优化 DOT”时保留 DOT。只有用户明确要求转换为 SVG，或确认了转换方案，才迁移格式。不要批量迁移旧图。

## 图文分工

动笔前写出一句内部信息契约：“读者看图后，应该立刻看懂什么空间关系？”图中只保留完成这句话所需的信息。

- 保留节点名、集合大小、方向、局部增减和边界位置。
- 删除文章标题、完整推导、结论段落、操作说明和重复图例。
- 单个中文标签尽量不超过 8 个汉字。
- 如果少量标签无法表达，优先拆图或改善正文，不增加说明框。

## 文件与引用

新图放在：

```text
book/pages/<article>/images/<descriptive-name>.svg
```

文件名使用描述性的英文 `kebab-case`。禁止 `1.svg`、`new.svg`、`final.svg`。

Markdown 使用站点根绝对路径和有意义的替代文本。文章页面使用无尾斜杠 URL，`./images/...` 会被浏览器解析到上一级目录；因此图片路径必须从 `book/pages/` 之后开始，并以 `/` 开头：

```markdown
![说明图中关键关系的替代文本](/<article>/images/<descriptive-name>.svg)
```

算法图不是装饰图，不使用空 `alt`。

## 制作流程

1. 阅读相邻正文，确定图的信息契约。
2. 选择 SVG 或 DOT；选择最接近的 SVG 模板。
3. 用 `viewBox` 建立 560 到 720 宽的固定浅色画布。
4. 使用内部 `<style>`、语义 class 和分组 `<g>` 编写图形。
5. 保留文本为 `<text>`；短数学只用 Unicode 符号和少量 `<tspan>`。
6. 添加 `<title>`、`<desc>`、`role="img"` 和 `aria-labelledby`。
7. 更新 Markdown 的站点根绝对路径引用与 `alt`。
8. 运行结构校验和双尺寸渲染：

```bash
node .agents/skills/rbook-svg-diagram/scripts/validate-svg.mjs <svg-path>
node .agents/skills/rbook-svg-diagram/scripts/render-svg.mjs <svg-path>
```

9. 使用本地图片查看器检查生成的 `desktop-720.png` 与 `mobile-360.png`。
10. 最后运行 rbook 的文章构建；第一版不把新校验接入全局 pre-check。

## 视觉验收

检查以下问题，而不是只确认“能打开”：

- 视线是否按预期顺序移动；
- 节点、边、箭头和标签是否对齐；
- 是否有文字或线条重叠；
- 颜色之外是否还有标签、箭头或线型表达语义；
- 360px 预览中关键文字是否仍接近 12px 且无需放大理解；
- 图是否比相邻正文更简洁，而不是重复正文。

## DOT 迁移

明确把旧 DOT 改为 SVG 时：

1. 在文章 `images/` 下创建新的语义化 SVG 文件。
2. 更新 Markdown 引用和 `alt`。
3. 完成校验、预览和构建。
4. 删除已经被替代的 `.dot`，避免两份来源并存；Git 历史负责恢复旧版。

## 完成条件

- 图形遵守 `references/design-system.md`。
- 校验脚本通过，无外部资源和不安全元素。
- 桌面与移动预览均已实际查看。
- Markdown 引用存在且 `alt` 有意义。
- 文章构建通过。
- 没有顺手迁移或重命名无关旧图。
