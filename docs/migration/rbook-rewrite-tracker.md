# Rbook 旧文重写追踪表

生成时间：2026-06-16

## 状态定义

| 状态 | 含义 |
|---|---|
| `raw-migrated` | 已从旧站点机械迁移，尚未按新教程结构重写 |
| `rewrite-ready` | 已确认目标路径和相近文章，可以开始重写 |
| `rewritten` | 文章已按 `rbook-article-writer` 的教程结构重写 |
| `code-normalized` | 可复用代码已迁入 `book/code/` 正式目录 |
| `verified` | 已完成 include 路径检查和 runtime 构建验证 |
| `archived` | 不进入主教程正文，只保留为归档或参考 |

## 当前基线

- 旧文已经机械迁移到 `book/pages/`，但大量内容仍是旧站点风格。
- `book/code/legacy/` 已删除；后续新增模板必须直接进入 `book/code/` 正式目录。
- 本项目不恢复 problem lookup，题目引用保留 `[[problem: oj,id]]` 或普通 Markdown 链接。

## Wave 1：高价值、低耦合主题

| 文章 | 状态 | 代码归位 | 备注 |
|---|---|---|---|
| `book/pages/base/presum/index.md` | `verified` | `/code/base/presum.cpp` | 已有高质量教程 |
| `book/pages/base/differential/index.md` | `verified` | `/code/base/differential.cpp`、`/code/base/differential_2d.cpp` | 已有高质量教程 |
| `book/pages/base/discrete/index.md` | `verified` | `/code/base/discrete离散化_unique.cpp` | 已有高质量教程 |
| `book/pages/base/binary_jump/index.md` | `rewritten` | `/code/base/binary_jump.cpp` | 已按高质量教程结构重写 |
| `book/pages/base/two-pointer/index.md` | `archived` | 使用正式 `base/double_point` | 旧目录保留为归档入口 |
| `book/pages/math/quick_pow/index.md` | `rewritten` | `/code/math/快速幂_quick_pow.cpp` | 已按高质量教程结构重写 |
| `book/pages/math/number_base/index.md` | `rewritten` | `/code/math/base_conversion/bin2dec.cpp`、`/code/math/base_conversion/dec2bin.cpp` | 已按高质量教程结构重写 |
| `book/pages/math/numberTheory/gcd/index.md` | `rewritten` | `/code/math/numberTheory/gcd.cpp` | 已按高质量教程结构重写 |
| `book/pages/graph/traversal/index.md` | `raw-migrated` | 待确认 | 需要补教程结构 |
| `book/pages/string/brute-force/index.md` | `rewritten` | `/code/string/brute_force_match.cpp` | 已按高质量教程结构重写 |
| `book/pages/string/kmp/index.md` | `rewritten` | `/code/string/kmp.cpp` | 已补前缀函数、证明和应用分类 |

## Legacy 引用清理

当前 `book/pages/` 中不应再出现 `@include-code(/code/legacy...)`。

| 文章 | 原 legacy 引用 | 新路径 | 状态 |
|---|---|---|---|
| `book/pages/utils/log.md` | `/code/legacy/utils/log.cpp` | `/code/utils/log.cpp` | `code-normalized` |
| `book/pages/graph/save/index.md` | `/code/legacy/graph/linklist.cpp` | `/code/graph/linklist.cpp` | `code-normalized` |
| `book/pages/data_structure/stack/index.md` | `/code/legacy/data_structure/stack.cpp` | `/code/data-struture/stack.cpp` | `code-normalized` |
| `book/pages/math/number_base/index.md` | `/code/legacy/math/base_conversion/*.cpp` | `/code/math/base_conversion/*.cpp` | `code-normalized` |
| `book/pages/recursion/dynamic_loop/index.md` | `/code/legacy/enumerate/递归实现多重循环.cpp` | `/code/base/enumerate/dynamic_loop.cpp` | `code-normalized` |
| `book/pages/math/numberTheory/gcd/index.md` | `/code/legacy/math/numberTheory/gcd.cpp` | `/code/math/numberTheory/gcd.cpp` | `code-normalized` |

## 后续批次

| 批次 | 范围 | 处理策略 |
|---|---|---|
| Wave 2 | 背包、LCS、LIS、数字三角形、斜率优化、四边形不等式优化 | 按专题成组重写，统一 DP 状态定义、转移、证明和模板 |
| Wave 2 | 线段树、单调队列、单调栈、并查集 | 把文章局部模板迁入 `book/code/data-struture/` |
| Wave 2 | 组合数学、递归与搜索 | 合并旧 `enumerate` 模板，避免重复题单堆叠 |
| Wave 3 | `data_structure`/`data-structure`、`tree`/`tree-algo` 等重复目录 | 逐主题决定合并、重定向或归档 |
| Wave 4 | appendix、problem_list、ReadingNotes、teach_plan | 只保留仍有维护价值的内容，过期内容归档 |

## 每批验收命令

```bash
rg "@include-code\\(/code/legacy" book/pages
test ! -e book/code/legacy
npm run build:packages
RBOOK_CONTENT_DIR=/home/rainboy/mycode/rbook_nunjucks/book \
  RBOOK_CODE_DIR=/home/rainboy/mycode/rbook_nunjucks/book/code \
  RBOOK_RUNTIME_DIR=/tmp/rbook-runtime-check \
  npm run build:runtime
```
