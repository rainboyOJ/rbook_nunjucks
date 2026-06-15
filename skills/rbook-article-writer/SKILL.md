---
name: rbook-article-writer
description: 在 rbook 算法电子书项目中编写、扩写或修改算法文章时必须使用这个 skill。用户要求写算法教程、完善草稿、整理题解、补充模板代码、修改 apps/algorithm-book/book 下文章、生成 code_template 元数据时都应触发。这个 skill 强制规定：核心算法代码、模板代码、可复用竞赛代码必须放在仓库根目录 code/ 下，并在 Markdown 中用 @include-code 引用。
---

# RBook 算法文章写作 Skill

这个 skill 用来指导 AI 在本项目中写算法电子书文章。目标不是写博客，而是写适合算法竞赛学习的电子书内容：概念清楚、心智负担低、代码可复用、风格和本书统一。

最重要的项目约束：

- 文章在 `apps/algorithm-book/book/` 下。
- 核心代码、模板代码、可复用竞赛代码在仓库根目录 `code/` 下。
- 文章中通过 `@include-code(/code/xxx.cpp, cpp)` 引用代码。
- 不要把完整可复用模板只塞在 Markdown 正文里。

## 必读上下文

开始编辑前，先按任务类型读取 `.github/prompts/` 中的本地 prompt。

- 总是读取 `.github/prompts/文章生成.prompt.md`。
- 创建或修改模板代码时读取 `.github/prompts/代码模板.prompt.md`。
- 完善草稿、题解、已有解析时读取 `.github/prompts/题解完善.prompt.md`。
- 写证明时读取 `.github/prompts/极简算式推导流.prompt.md` 和 `.github/prompts/符合人脑证明.md`。
- 写“应用分类详解”时读取 `.github/prompts/应用分类详解/` 下的相关示例，例如二分查找、树状数组。

还要阅读项目内相近文章：

- 优先看同目录或同主题文章。
- 简洁证明风格可以参考 `apps/algorithm-book/book/graph/center_of_tree/index.md`。
- `code_template` 的 front matter 写法可以参考树状数组、快速排序、BCC、SCC 等已有文章。

## 工作流程

1. 确定目标文章路径。
   - 用户给了草稿时，不要直接覆盖草稿，除非用户明确要求。
   - 默认把成品写到 `apps/algorithm-book/book/<topic>/index.md`。
   - 如果目标文章已经存在，保留有价值内容，在原文件上改进。

2. 收集项目上下文。
   - 在 `apps/algorithm-book/book/` 里搜索相关讲解。
   - 在 `code/` 里搜索已有代码，避免重复创建模板。
   - 需要把文章加入目录时，再检查 `apps/algorithm-book/book.yaml`。

3. 判断哪些代码必须进入 `code/`。
   - 核心算法实现必须进入 `code/`。
   - 可复用竞赛模板必须进入 `code/`。
   - 完整可运行 C++ 程序如果能作为模板或典型实现，也必须进入 `code/`。
   - 文章本地的 `apps/algorithm-book/book/.../code/` 只适合非常小、不可复用的一次性演示。
   - 如果草稿中有完整模板代码，或文章本地 code 目录中有可复用模板，应移动或复制到合适的根目录 `code/` 路径，然后更新文章引用。

4. 写文章。
   - 默认使用中文。
   - 不使用 emoji。
   - 不写废话，不写营销式开头。
   - 先讲问题模型，再讲直觉，再讲步骤、证明、代码。
   - 使用低心智负担的解释：类比可以用，但不能牺牲准确性。
   - 可以使用 `!!! definition`、`!!! note` 等 admonition 语法强调关键概念。

5. 引用代码。
   - 推荐写法：
     ```markdown
     @include-code(/code/<domain>/<file>.cpp, cpp)
     ```
   - 已有文章也有 `@include-code(code/<domain>/<file>.cpp, cpp)`，可以兼容。
   - 不要在 Markdown 中粘贴整份可复用 C++ 模板。
   - 短伪代码、几行辅助代码、推导片段可以直接写在正文中。

6. 验证。
   - 检查每个 `@include-code` 路径是否真实存在。
   - 检查 front matter 是否是合法 YAML。
   - 对文章/代码改动，优先运行：
     ```bash
     npm run build:packages
     RBOOK_CONTENT_DIR=/home/rainboy/mycode/rbook_nunjucks/apps/algorithm-book \
       RBOOK_CODE_DIR=/home/rainboy/mycode/rbook_nunjucks/code \
       RBOOK_RUNTIME_DIR=/tmp/rbook-runtime-check \
       npm run build:runtime
     ```

## 推荐文章结构

根据文章主题灵活调整，但算法教程通常使用这个骨架：

```markdown
---
id: "<short-id>"
title: "<中文标题>"
date: YYYY-MM-DD HH:mm
toc: true
tags: ["<算法名>"]
categories: ["<分类>"]
code_template:
  - title: <模板名>
    desc: "<简短说明>"
    tags: ["<标签>"]
    code: /code/<domain>/<file>.cpp
---

[[TOC]]

## 一句话算法

## 问题模型

## 核心直觉

## 算法步骤

## 算法证明

## 复杂度分析

## 代码实现

@include-code(/code/<domain>/<file>.cpp, cpp)

## 测试用例

## 应用分类详解

## 经典例题

## 参考
```

如果文章没有可复用代码，可以省略 `code_template` 和 `## 代码实现`。

## 代码放置规则

根据主题选择根目录 `code/` 下的位置：

- 基础算法：`code/base/`
- 数据结构：`code/data-struture/`，保留项目中已有拼写
- 图论：`code/graph/`
- 树上算法：`code/tree/`
- 数学与数论：`code/math/`
- 字符串：`code/string/`
- 通用模板：`code/template/`
- 工具函数：`code/utils/`

新增代码文件时：

- 使用清晰文件名，并尽量贴合现有命名风格。
- C++ 竞赛模板优先使用 `.cpp`。
- 模板要完整、可读，适合竞赛时直接复制。
- 注释解释不变量、边界、容易写错的位置。
- 不要过度封装；优先写心智负担低的 `struct` 或函数。

如果这份代码应出现在代码模板 UI 中，文章 front matter 中要增加 `code_template`：

```yaml
code_template:
  - title: 树状数组
    desc: "单点修改,区间查询"
    tags: ["树状数组", "区间信息"]
    code: /code/data-struture/BIT/bit.cpp
```

## 应用分类详解怎么写

“应用分类详解”不是普通总结，它的任务是告诉读者：

- 这个算法能解决哪几类问题。
- 每一类问题有什么可识别的题面特征。
- 为什么这个算法适合这一类问题。
- 遇到题目时应该如何判断“这是它能做的问题”。

写这一节前，先读 `.github/prompts/应用分类详解/` 下已有示例。示例里的二分查找展示了推荐形态：先给算法本质，再按应用模型分层，每层说明典型模式、经典题目、核心建模。

推荐结构：

```markdown
## 应用分类详解

<一句话说明算法本质，例如：二分是在有单调性的空间中不断排除一半候选。>

### 一、<类别名：例如 基础定位 / 边界查找>

**典型模式：** <题面或数据结构特征>
**识别信号：** <看到哪些关键词、约束、操作时应该想到它>
**核心建模：** <如何把题目转成这个算法的问题>

| 应用场景 | 经典题目 | 核心思路 |
|---------|---------|---------|
| <场景> | <题目链接或 [[problem: oj,id]]> | <一句话说明为什么适用> |
```

分类时不要按“题号列表”堆题，而要按问题模型分类。例如：

- 二分查找可分为：有序序列定位、特殊数组查找、二分答案、数值解、作为其他算法的加速组件。
- 树状数组可分为：单点修改区间查询、区间修改单点查询、逆序对统计、离线计数、二维偏序、权值树状数组。
- FHQ-Treap 可分为：按值维护集合、按排名维护序列、区间翻转、区间移动、批量分裂合并。

每个分类都应回答三个问题：

1. 题面长什么样？
2. 这个算法维护了什么信息？
3. 操作为什么能高效完成？

避免这些问题：

- 只列题目，不解释问题模型。
- 把同一种模型拆成很多重复小类。
- 写成泛泛的“可以优化复杂度”，但不说明优化的是哪个操作。
- 为了凑数量硬列不适合的应用。

## 写作标准

- `## 一句话算法` 要像口诀或记忆钩子，用一句话抓住核心思想。
- `## 问题模型` 要说明输入结构、操作、查询、约束和目标。
- `## 核心直觉` 先讲人能记住的模型，再给形式化描述。
- `## 算法证明` 要短、直观、低心智负担：
  - 先指出关键不变量；
  - 再说明每一步为什么保持不变量；
  - 数学证明使用极简算式推导流。
- `## 复杂度分析` 必须包含时间复杂度和空间复杂度。
- `## 测试用例` 至少包含一个普通用例；容易错的算法应补边界用例。
- `## 经典例题` 不只是列链接，还要说明这道题为什么适合该算法。

## 题目链接

本项目不负责题目数据查询。题目数据已经拆到独立项目或服务中。

写题目引用时：

- 已有 `[[problem: oj,id]]` 语法可以保留。
- 不要在本项目中重新实现 problem lookup。
- 已知 URL 时可以使用普通 Markdown 链接。
- 不确定题目信息时不要编造。

## 完成前检查清单

结束前逐项检查：

- 成品文章没有误覆盖草稿文件，除非用户明确要求。
- 可复用模板代码没有只存在于 Markdown 中。
- 核心代码、模板代码已经放到仓库根目录 `code/`。
- `@include-code` 引用路径存在。
- `code_template` 中的路径和真实代码文件一致。
- `toc: true` 的文章包含 `[[TOC]]`。
- “应用分类详解”按问题模型分类，而不是只堆题目。
- 文章风格简洁、准确，和算法电子书一致。
