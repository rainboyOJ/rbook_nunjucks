# Rbook_ejs_old 文章迁移盘点报告

生成时间：2026-06-16

## 目标

把 `Rbook_ejs_old/` 中仍有价值的旧文章逐步迁移到当前项目，并按 `skills/rbook-article-writer/SKILL.md` 的规则改写：

- 成品文章放到 `book/pages/`。
- 核心算法代码、模板代码、可复用竞赛代码放到 `book/code/`。
- Markdown 中使用 `@include-code(/code/xxx.cpp, cpp)` 引用代码。
- 不把完整可复用模板只保留在文章正文或文章局部 `code/` 目录里。
- 算法教程需要补齐“应用分类详解”，说明算法能解决哪几类问题、识别信号和建模方式。

本报告只做盘点和迁移策略，不执行文章迁移。

## 范围

本轮盘点范围：

- 旧文章目录：`Rbook_ejs_old/book/`
- 旧模板目录：`Rbook_ejs_old/algo_template/`
- 当前文章目录：`book/pages/`
- 当前代码目录：`book/code/`

暂不把 `Rbook_ejs_old/src/`、`Rbook_ejs_old/bin/`、`Rbook_ejs_old/.parcel-cache/`、旧构建脚本、旧站点框架代码作为迁移对象。后续如果发现文章依赖这些目录里的资源，再按具体文章补充处理。

## 总量统计

| 项目 | 数量 |
|---|---:|
| 旧 Markdown 文章 | 249 |
| 旧目录总文件数 | 700 |
| 当前 Markdown 文章 | 241 |
| 旧 `algo_template` 文件 | 28 |
| 当前 `book/code` 文件 | 95 |
| 旧文章与当前文章相同相对路径 | 30 |
| 旧文章相对路径未出现在当前项目 | 219 |
| 旧 `algo_template` 与当前 `book/code` 相同相对路径 | 2 |

## 旧文章目录分布

未按相同路径出现在当前 `book/pages/` 的旧 Markdown 主要分布如下：

| 顶层目录 | 未迁移 Markdown 数 |
|---|---:|
| `appendix` | 35 |
| `math` | 33 |
| `data_structure` | 29 |
| `recursion` | 25 |
| `dynamic_programming` | 25 |
| `base` | 20 |
| `graph` | 12 |
| `string` | 6 |
| `problem_list` | 6 |
| `utils` | 4 |
| `tree` | 4 |
| `algorithm` | 3 |
| `greedy` | 3 |
| `ReadingNotes` | 3 |
| `sort` | 2 |
| `tricks` | 2 |
| 其他零散文件或目录 | 5 |

结论：迁移主体不是零散文章，而是几组系统主题：基础算法、数据结构、动态规划、图论、数学、递归与搜索。

## 重复内容判断

### 相同路径

旧文章和当前文章有 30 个相同相对路径。其中 26 个内容完全相同，4 个路径相同但内容不同。

内容完全相同的路径主要集中在：

- `enumeration_permutaion_combination/01_sequence/`
- `enumeration_permutaion_combination/ball_and_box/`
- `enumeration_permutaion_combination/permutation/`
- `enumeration_permutaion_combination/stack_seq/`
- `enumeration_permutaion_combination/subset_enum/index.md`

这些文件不应直接覆盖当前文件。后续只需要检查它们是否符合新文章规范，例如代码是否已经进入 `book/code/`、题目链接是否仍能解析、是否需要补“应用分类详解”。

路径相同但内容不同：

- `about.md`
- `enumeration_permutaion_combination/pair_number/index.md`
- `enumeration_permutaion_combination/pair_number/teach_plan.md`
- `graph/topsort/index.md`

这些属于合并候选，不能简单复制旧文件覆盖当前文件。

### 内容哈希重复

旧文章内部存在 18 个空 Markdown 文件，它们和当前项目的一些空文件哈希相同。这个结果只是说明文件为空，不代表已经完成迁移。空文件应按“跳过或重写”处理。

旧文章中还有部分内容已经以不同路径迁入当前项目，例如：

- `recursion/binary_search/problem.md` 对应当前 `base/binary_search/problem.md`
- `recursion/binary_search/history.md` 对应当前 `base/binary_search/history.md`
- `recursion/binary_search/teach_plan.md` 对应当前 `base/binary_search/teach_plan.md`
- `dynamic_programming/sparse_table/index.md` 对应当前 `base/sparse_table/草稿.md`
- `dynamic_programming/sparse_table/problem.md` 对应当前 `base/sparse_table/problem.md`

这些应按“已迁移但路径变化”处理，后续只做质量检查和规范化。

## 旧格式依赖

旧文章大量使用 EJS 宏和旧站点能力，不能机械复制。

| 信号 | 涉及 Markdown 数 | 迁移处理 |
|---|---:|---|
| `<%- ... %>` EJS 宏 | 94 | 必须改写或删除 |
| `pid_to_url(...)` 题目链接 | 14 | 改成 `[[problem: oj,id]]` 或普通 Markdown 链接 |
| `video(...)` | 4 | 迁移为普通媒体链接，或暂时删除并标 TODO |
| `iframe(...)` | 6 | 需要确认当前站点是否保留对应交互页面 |
| Obsidian `[[...]]` / `![[...]]` | 49 | 改为当前项目可解析的 Markdown 链接或资源路径 |
| 图片语法 `![](...)` / `<img>` | 34 | 需要迁移图片并校验路径 |
| C++ fenced code | 79 | 判断是否应抽到 `book/code/` |

重点风险：

- `<%- include("./xxx.md") %>`：如果是文章片段，可以内联合并或改成当前支持的 include 语法；如果是题面/练习列表，优先合并为正文。
- `<%- include("./code/xxx.cpp") %>`：可复用代码必须迁入 `book/code/`，正文改为 `@include-code(/code/..., cpp)`。
- `<%- include("/algo_template/xxx.cpp") %>`：优先从 `Rbook_ejs_old/algo_template/` 或当前 `book/code/` 找模板，迁入当前规范路径。
- `pid_to_url(...)`：本项目不重新实现 problem lookup，保留 `[[problem: oj,id]]` 或已知 URL。
- `![[...]]`：这是 Obsidian 嵌入格式，当前站点未必能解析，需要改成普通图片或链接。
- `iframe("/canvas/...")`：如果当前项目没有对应 canvas 页面，需要先决定是否迁移交互演示。

## 代码和资源迁移

旧文章目录中检测到大量局部代码和图片资源。按顶层目录统计，代码类资源主要在：

| 顶层目录 | 代码类文件数 |
|---|---:|
| `data_structure` | 30 |
| `dynamic_programming` | 29 |
| `enumeration_permutaion_combination` | 22 |
| `recursion` | 12 |
| `base` | 9 |
| `appendix` | 9 |
| `graph` | 7 |
| `sort` | 7 |
| `utils` | 6 |
| `math` | 4 |
| `string` | 3 |

图片和图示资源主要在：

| 顶层目录 | 图片/图示文件数 |
|---|---:|
| `dynamic_programming` | 30 |
| `appendix` | 9 |
| `sort` | 8 |
| `recursion` | 7 |
| `data_structure` | 6 |
| `graph` | 4 |
| `other` | 4 |
| `base` | 3 |
| `enumeration_permutaion_combination` | 3 |
| `math` | 3 |
| `string` | 2 |
| `tree` | 2 |

旧 `algo_template` 与当前 `book/code` 只有两个相同路径：

- `graph/linklist.cpp`
- `readme.md`

其余旧模板需要逐个判断是否迁入当前 `book/code/`，尤其是：

- `base/presum.cpp`
- `data_structure/queue.cpp`
- `data_structure/stack.cpp`
- `enumerate/*.cpp`
- `math/base_conversion/*.cpp`
- `math/combinatorics/*.cpp`
- `math/numberTheory/gcd.cpp`
- `utils/log.cpp`
- `utils/random.cpp`

迁移代码时应同时补文章 front matter 的 `code_template`，让代码模板 UI 能展示。

## 建议迁移分组

### 第一批：高价值、低耦合算法教程

优先迁移这些主题，因为它们是算法书正文内容，且通常能独立改写：

- `base/presum/index.md`
- `base/differential/index.md`
- `base/discrete/index.md`
- `base/binary_jump/index.md`
- `base/two-pointer/index.md`
- `math/quick_pow/index.md`
- `math/number_base/index.md`
- `math/numberTheory/gcd/index.md`
- `graph/topsort/index.md`
- `graph/traversal/index.md`
- `string/brute-force/index.md`
- `string/kmp/index.md`

处理方式：

- 保留有价值讲解，但按新文章结构重写。
- 抽取模板代码到 `book/code/`。
- 补“应用分类详解”。
- 题目引用改成 `[[problem: oj,id]]` 或普通 Markdown 链接。

### 第二批：系统性专题，需要成组迁移

这些目录文章之间引用较多，建议按专题整组处理：

- `dynamic_programming/knapsack/`
- `dynamic_programming/lcs/`
- `dynamic_programming/lis/`
- `dynamic_programming/number_pyramid/`
- `dynamic_programming/slope/`
- `dynamic_programming/Quadrangle_Inequality_Optimization/`
- `data_structure/segment_tree/`
- `data_structure/monotonic_queue/`
- `data_structure/monotonic_stack/`
- `data_structure/disjoint_set/`
- `math/combinatorics/`
- `recursion/`

处理方式：

- 先确定当前项目是否已有同主题文章，避免重复目录。
- 把旧文章里的 `problem.md`、`practice.md`、`todo.md` 合并成正文中的“经典例题”或“练习”。
- 把多份代码按模板、演示、题解代码分类：模板进 `book/code/`，一次性演示可留在文章附近或直接内联短片段。

### 第三批：合并或归档

这些内容不一定适合作为算法正文直接迁移：

- `appendix/SoftWare/`
- `appendix/program_environment/`
- `appendix/shellScripts/`
- `problem_list/`
- `ReadingNotes/`
- `teach_plan.md`
- `introducation/`
- `mind_theory/`

建议策略：

- 和算法学习强相关的内容可以迁到 `appendix/`。
- 软件安装、环境配置类内容容易过期，迁移前需要重新验证，不建议直接复制。
- 题单类内容要确认是否仍由本项目维护；如果题目数据已拆分到独立项目，尽量不要在本项目恢复旧题库逻辑。
- 读书笔记、教学计划优先归档，不作为第一批迁移。

## 迁移操作规则

每迁移一篇文章，按这个顺序执行：

1. 确定目标路径，例如 `book/pages/base/presum/index.md`。
2. 检查当前项目是否已有同主题文章。
3. 阅读相邻文章，保持当前项目风格。
4. 识别旧文里的 EJS 宏、局部代码、图片、题目链接。
5. 可复用代码迁入 `book/code/<domain>/`。
6. Markdown 中使用 `@include-code(/code/<domain>/<file>.cpp, cpp)`。
7. 需要展示到模板 UI 的，在 front matter 中补 `code_template`。
8. 改写或合并旧 `problem.md`、`practice.md`。
9. 补齐“应用分类详解”。
10. 检查所有 `@include-code` 路径和图片路径。
11. 运行构建验证。

推荐验证命令：

```bash
npm run build:packages
RBOOK_CONTENT_DIR=/home/rainboy/mycode/rbook_nunjucks/book \
  RBOOK_CODE_DIR=/home/rainboy/mycode/rbook_nunjucks/book/code \
  RBOOK_RUNTIME_DIR=/tmp/rbook-runtime-check \
  npm run build:runtime
```

## 建议下一步

下一步不要全量迁移。建议先选一个低耦合主题做样板，验证迁移格式和构建链路。

推荐第一个样板：

- `base/presum/index.md`

原因：

- 主题独立。
- 旧文使用了 `/algo_template/base/presum.cpp`，能验证代码迁入 `book/code/` 的流程。
- 旧文包含 `pid_to_url(...)`，能验证题目链接改写规则。
- 文章规模适中，适合作为后续批量迁移的模板。

样板迁移完成后，再按“第一批”列表继续迁移基础算法和数学主题。

## 批量迁移执行记录

执行时间：2026-06-16

- 迁移 Markdown：216 篇。
- 复制旧文章依赖资源和样例文件：430 个。
- 旧 `algo_template` 已复制到 `book/code/legacy/`，用于兼容旧文中的绝对 include。
- 已在 `book/book.yaml` 的 `glob` 中加入精确文件路径，避免渲染无关草稿。
- 当前策略不覆盖已有文件；路径冲突文件保留当前项目版本。

### 批量迁移验证

验证时间：2026-06-16

- 旧 EJS 宏残留检查通过：`<%- ... %>`、`pid_to_url(...)`、`video(...)`、`iframe(...)`、`![[...]]` 均无残留。
- `@include_md(...)` 文件存在性检查通过。
- `@include-code(...)` 文件存在性检查通过。
- `npm run build:packages` 通过。
- `RBOOK_CONTENT_DIR=/home/rainboy/mycode/rbook_nunjucks/book RBOOK_CODE_DIR=/home/rainboy/mycode/rbook_nunjucks/book/code RBOOK_RUNTIME_DIR=/tmp/rbook-runtime-check npm run build:runtime` 通过。
- runtime 搜索索引结果：`pages=429`，`chunks=2896`，`errors=0`。

遗留说明：

- 这次是“机械全量迁移”，目标是把旧内容完整搬入当前项目并保证可构建。
- 已迁移旧文还没有全部按 `rbook-article-writer` 的高质量教程结构重写。
- 部分旧代码为了兼容旧文章引用，暂放在 `book/code/legacy/`；后续应按主题逐步整理到 `book/code/base/`、`book/code/graph/`、`book/code/math/` 等正式目录。

## Legacy 代码清理执行记录

执行时间：2026-06-16

- 新增长期追踪表：`docs/migration/rbook-rewrite-tracker.md`。
- 已移除 `book/pages/` 中所有 `@include-code(/code/legacy...)` 引用。
- 已将仍有价值的旧模板迁入正式 `book/code/` 目录：
  - `book/code/base/enumerate/`
  - `book/code/data-struture/`
  - `book/code/math/base_conversion/`
  - `book/code/math/numberTheory/`
  - `book/code/utils/`
- 已删除 `book/code/legacy/`。后续新增或整理模板时，不再使用 legacy 目录。
- 同步修复 4 处非 legacy 的 `@include-code` 相对路径断链。

### 本轮验证

- `book/pages/` 中 legacy include 扫描通过。
- `@include-code(...)` 文件存在性检查通过：105 个含代码引用的文章全部可解析到真实文件。
- 新增 C++ 模板使用 `g++ -std=c++17 -Wall -Wextra -pedantic -c` 通过语法检查。
- `npm run build:packages` 通过。
- `RBOOK_CONTENT_DIR=/home/rainboy/mycode/rbook_nunjucks/book RBOOK_CODE_DIR=/home/rainboy/mycode/rbook_nunjucks/book/code RBOOK_RUNTIME_DIR=/tmp/rbook-runtime-check npm run build:runtime` 通过。
- runtime 搜索索引结果：`pages=429`，`chunks=2901`，`errors=0`。
