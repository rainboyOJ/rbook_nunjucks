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
| `book/pages/base/binary_jump/index.md` | `verified` | `/code/base/binary_jump.cpp` | 已按高质量教程结构重写，旧 `倍增思想.md` 保留为归档入口 |
| `book/pages/base/two-pointer/index.md` | `archived` | 使用正式 `base/double_point` | 旧目录保留为归档入口 |
| `book/pages/math/quick_pow/index.md` | `verified` | `/code/math/快速幂_quick_pow.cpp` | 已按高质量教程结构重写，旧局部模板已删除 |
| `book/pages/math/number_base/index.md` | `verified` | `/code/math/base_conversion/bin2dec.cpp`、`/code/math/base_conversion/dec2bin.cpp` | 已按高质量教程结构重写 |
| `book/pages/math/numberTheory/gcd/index.md` | `verified` | `/code/math/numberTheory/gcd.cpp` | 已按高质量教程结构重写 |
| `book/pages/graph/traversal/index.md` | `verified` | `/code/graph/dfs_traversal.cpp`、`/code/graph/connected_components.cpp` | 已按高质量教程结构重写，`graph/dfs` 保留为归档入口 |
| `book/pages/string/brute-force/index.md` | `verified` | `/code/string/brute_force_match.cpp` | 已按高质量教程结构重写，旧局部模板已删除 |
| `book/pages/string/kmp/index.md` | `verified` | `/code/string/kmp.cpp` | 已补前缀函数、证明和应用分类 |

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

## 已完成提交记录

| 提交 | 阶段 |
|---|---|
| `7f91951` | 重写进制转换和 gcd 教程 |
| `1f7bfa3` | 重写快速幂和倍增跳跃教程 |
| `c26cfdf` | 重写字符串朴素匹配和 KMP 教程 |
| `4ca99cd` | 重写图遍历教程 |
| `64fa0f0` | 清理已迁入 `book/code/` 的旧页面局部模板 |

## 后续批次

| 批次 | 范围 | 处理策略 |
|---|---|---|
| Wave 2A | `dynamic_programming/knapsack/01knapsack`、`full_knapsack`、`grouped_knapsack` | 统一背包问题模型、状态定义、转移方向和模板路径 |
| Wave 2A-1 | `dynamic_programming/knapsack/01knapsack` | 已验证：新增 `/code/dynamic_programming/knapsack/zero_one_*.cpp`，重写 01 背包教程 |
| Wave 2A-2 | `dynamic_programming/knapsack/full_knapsack` | 已验证：新增 `/code/dynamic_programming/knapsack/complete_*.cpp`，重写完全背包教程 |
| Wave 2A-3 | `dynamic_programming/knapsack/grouped_knapsack` | 已验证：新增 `/code/dynamic_programming/knapsack/grouped.cpp`，重写分组背包教程 |
| Wave 2B | `dynamic_programming/lcs`、`lis`、`number_pyramid` | 按“状态表示、转移来源、边界、滚动优化”重写 DP 入门线 |
| Wave 2B-1 | `dynamic_programming/number_pyramid` | 已验证：新增 `/code/dynamic_programming/number_pyramid.cpp`，重写数字三角形教程 |
| Wave 2B-2 | `dynamic_programming/lcs` | 已验证：新增 `/code/dynamic_programming/lcs.cpp`、`/code/dynamic_programming/lcs_restore.cpp`，清理页面局部模板 |
| Wave 2B-3 | `dynamic_programming/lis` | 已验证：新增 `/code/dynamic_programming/lis_n2.cpp`、`/code/dynamic_programming/lis_binary.cpp`，清理页面局部模板 |
| Wave 2C | `data_structure/monotonic_queue`、`monotonic_stack`、`heap`、`disjoint_set` | 把页面局部模板迁入 `book/code/data-struture/`，清理旧题单式正文 |
| Wave 2C-1 | `data_structure/monotonic_queue` | 已验证：新增 `/code/data-struture/monotonic_queue_sliding_window.cpp`、`/code/data-struture/monotonic_queue_max_subarray.cpp`，清理页面局部模板和对拍文件 |
| Wave 2C-2 | `data_structure/monotonic_stack` | 已验证：新增 `/code/data-struture/monotonic_stack_next_greater.cpp`、`/code/data-struture/monotonic_stack_largest_rectangle.cpp`，清理页面局部模板和坏图引用 |
| Wave 2C-3 | `data_structure/heap` | 已验证：复用并修正 `/code/data-struture/heap堆.cpp`，清理页面局部模板 |
| Wave 2D | `base/sparse_table`、`dynamic_programming/sparse_table` | 合并重复 ST 表内容，保留一个正式教程，另一个归档 |
| Wave 3A | `data_structure`/`data-structure`、`tree`/`tree-algo` 重复目录 | 逐主题决定合并、重定向或归档 |
| Wave 3B | `enumeration_permutaion_combination`、`recursion`、`search` | 统一枚举/递归/搜索模板，避免相互引用旧 rbook 名称 |
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
