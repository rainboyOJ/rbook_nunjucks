---
name: rbook-article-writer
description: Write, expand, or revise algorithm ebook articles in this rbook repository. Use this whenever the user asks to create an algorithm article, improve a draft, write a tutorial, add a template implementation, or edit pages under apps/algorithm-book/book. This skill enforces the project rule that core algorithms and reusable contest templates belong in the repository root code/ directory and must be referenced from Markdown with @include-code.
---

# RBook Article Writer

Use this skill to write or revise articles for this algorithm ebook. The ebook is a competitive-programming oriented knowledge base, not a blog: articles should teach an algorithm clearly, preserve the book's style, and leave reusable code in the shared `code/` tree.

## Required Context

Before editing, read the relevant local prompts in `.github/prompts/`:

- Always read `.github/prompts/文章生成.prompt.md`.
- Read `.github/prompts/代码模板.prompt.md` when creating or changing code.
- Read `.github/prompts/题解完善.prompt.md` when improving a draft or problem explanation.
- Read `.github/prompts/极简算式推导流.prompt.md` and `.github/prompts/符合人脑证明.md` when writing proofs.
- Read `.github/prompts/应用分类详解/*.md` when the article needs a "when to use this algorithm" section.

Also inspect nearby existing articles before writing:

- Prefer articles in the same topic directory.
- Use `apps/algorithm-book/book/graph/center_of_tree/index.md` as a concise proof/style reference.
- Use articles with `code_template` front matter as references for code template metadata.

## Core Workflow

1. Identify the target article path.
   - If the user gives a draft, do not overwrite the draft unless explicitly requested.
   - Normally write the finished article to `apps/algorithm-book/book/<topic>/index.md`.
   - If an article already exists, preserve useful existing content and improve it in place.

2. Gather context from the repository.
   - Search `apps/algorithm-book/book/` for related explanations.
   - Search `code/` for existing implementations before writing new code.
   - Check `apps/algorithm-book/book.yaml` if the user asks to place the article in navigation.

3. Decide which code is reusable.
   - Core algorithm implementations, templates, and complete runnable C++ programs must live under repository root `code/`.
   - Article-local `apps/algorithm-book/book/.../code/` files are only acceptable for small one-off demonstrations that are not reusable templates.
   - If a draft contains a reusable implementation inline or under article-local `code/`, move it to an appropriate root `code/` path and update the article.

4. Write or update the article.
   - Use clear Chinese unless the user asks otherwise.
   - Avoid emoji and filler.
   - Use low-mental-load explanations, concrete examples, and compact proofs.
   - Use admonitions such as `!!! definition` when they clarify a concept.

5. Reference code from Markdown.
   - Use `@include-code(/code/<domain>/<file>.cpp, cpp)` or `@include-code(code/<domain>/<file>.cpp, cpp)`.
   - Do not paste full reusable C++ templates directly into Markdown.
   - Small pseudocode fragments and very short illustrative snippets may be inline.

6. Validate.
   - Confirm every `@include-code` path exists.
   - Confirm front matter is valid YAML.
   - Run the narrowest reasonable build check. For article/code changes, prefer:
     ```bash
     npm run build:packages
     RBOOK_CONTENT_DIR=/home/rainboy/mycode/rbook_nunjucks/apps/algorithm-book \
       RBOOK_CODE_DIR=/home/rainboy/mycode/rbook_nunjucks/code \
       RBOOK_RUNTIME_DIR=/tmp/rbook-runtime-check \
       npm run build:runtime
     ```

## Article Shape

Use this structure unless the local article style or user request clearly calls for a different one:

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

If the article does not need a reusable template, omit `code_template` and `## 代码实现`.

## Code Placement Rules

Choose root `code/` paths by topic:

- Basic algorithms: `code/base/`
- Data structures: `code/data-struture/` (preserve this existing spelling)
- Graph algorithms: `code/graph/`
- Tree algorithms: `code/tree/`
- Math/number theory: `code/math/`
- String algorithms: `code/string/`
- General templates: `code/template/`
- Utilities: `code/utils/`

For new files:

- Use descriptive lowercase or existing local naming style.
- Prefer `.cpp` for C++17 contest templates.
- Keep templates complete enough to compile or clearly mark them as snippets in the filename/comment.
- Add concise comments that explain invariants and tricky lines.
- Avoid over-engineered abstractions; prefer contest-readable structs/functions.

When adding code, also add `code_template` front matter if the implementation is meant to be discovered from the code template UI:

```yaml
code_template:
  - title: 树状数组
    desc: "单点修改,区间查询"
    tags: ["树状数组", "区间信息"]
    code: /code/data-struture/BIT/bit.cpp
```

## Writing Standards

- Start from the problem model: what input structure, what operation, what query, what invariant.
- Include `## 一句话算法`: one memorable sentence or口诀 that captures the algorithm.
- Prefer "直觉模型 -> 步骤 -> 证明 -> 代码" over dumping formulas first.
- Proofs should be short and brain-friendly:
  - name the invariant;
  - explain why each step preserves it;
  - use a minimal derivation flow for math-heavy arguments.
- Complexity analysis must include time and space.
- Test cases should include at least one normal case and one boundary or failure-prone case when useful.
- Classic problems should include links and a short "why this algorithm fits" note.

## Problem Links

Do not implement problem lookup in this project. Problem data has been split into a separate project/service.

If the article needs problem references:

- Preserve existing `[[problem: oj,id]]` syntax when it already appears.
- Do not invent problem metadata.
- Use ordinary Markdown links only when the URL is known.

## Final Checklist

Before finishing, check:

- The final article is not the draft file unless requested.
- No reusable template code remains only inside Markdown.
- Core/template code files are under repository root `code/`.
- `@include-code` references point to existing files under `code/`.
- `code_template` entries match the actual code paths.
- The article has `[[TOC]]` when `toc: true`.
- The article is concise, practical, and consistent with the algorithm ebook style.
