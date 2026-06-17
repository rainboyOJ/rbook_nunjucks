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
| Wave 2C-4 | `data_structure/disjoint_set` | 已验证：新增 `/code/data-struture/dsu.cpp`、`/code/data-struture/dsu_bipartite.cpp`，补齐普通并查集和种类并查集教程 |
| Wave 2D | `base/sparse_table`、`dynamic_programming/sparse_table` | 合并重复 ST 表内容，保留一个正式教程，另一个归档 |
| Wave 2D-1 | `base/sparse_table`、`dynamic_programming/sparse_table` | 已验证：正式教程保留在 `base/sparse_table`，`dynamic_programming/sparse_table` 改为归档入口，统一模板 `/code/base/sparse_table.cpp` |
| Wave 3A | `data_structure`/`data-structure`、`tree`/`tree-algo` 重复目录 | 逐主题决定合并、重定向或归档 |
| Wave 3A-1 | `data_structure/stack`、`data_structure/queue`、`data_structure/binary_search_tree` | 已验证：正式教程迁移到 `data-structure/stack`、`data-structure/queue`、`data-structure/BST`，旧路径改为归档入口，复用 `/code/data-struture/stack.cpp`、`queue.cpp`、`bst.cpp` |
| Wave 3A-2 | `tree/jump-lca`、`tree-algo/jump-lca` | 已验证：新增正式教程 `tree-algo/jump-lca`，旧 `tree/jump-lca` 改为归档入口，局部 `template.cpp` 迁入 `/code/tree/jump_lca.cpp` |
| Wave 3B | `enumeration_permutaion_combination`、`recursion`、`search` | 统一枚举/递归/搜索模板，避免相互引用旧 rbook 名称 |
| Wave 3B-1 | `recursion/dynamic_loop`、`recursion/combination.md`、`enumeration_permutaion_combination/permutation/full_permutation`、`subset_enum` | 已验证：重写多重循环、组合、全排列、子集枚举基础教程，新增 `/code/base/enumerate/subset_binary.cpp`，清理对应页面局部模板和生成产物 |
| Wave 3B-2 | `recursion/print_to_n`、`fibonacci`、`hanoi`、`div_number` | 已验证：新增 `/code/base/recursion/*.cpp` 递归基础模板，重写递归前进回溯、斐波那契、汉诺塔、整数划分教程，清理页面局部模板 |
| Wave 3B-3 | `recursion/add_to_n`、`recursion/binary_search`、`enumeration_permutaion_combination/01_sequence` | 已验证：新增递归求和模板，重写递归求和与 01 序列教程，`recursion/binary_search` 改为归档入口，清理对应页面局部模板 |
| Wave 3B-4 | `enumeration_permutaion_combination/permutation/not_repeat_permutation`、`non_recursive_permutation` | 已验证：新增 `/code/base/enumerate/multiset_permutation.cpp`、`next_permutation_manual.cpp`，重写重复元素排列和字典序非递归排列教程，清理页面局部旧模板和生成产物 |
| Wave 3B-5 | `enumeration_permutaion_combination/stack_seq` | 已验证：新增 `/code/base/enumerate/stack_output_count.cpp`，重写出栈序列计数教程，清理页面局部暴力代码、演示代码和生成产物 |
| Wave 3B-6 | `enumeration_permutaion_combination/ball_and_box` | 已验证：新增 `/code/math/combinatorics/stirling_second_*.cpp`，重写球盒模型总览和第二类 Stirling 数教程，旧 `index_old` 改归档，清理页面局部代码 |
| Wave 3B-7 | `enumeration_permutaion_combination/pair_number` | 已验证：新增 `/code/base/pair_count/*.cpp`，重写配对问题教程，覆盖属性、Two Sum、同余、距离限制和单调队列最值模型，清理旧局部代码与草稿 |
| Wave 4A | `dynamic_programming/数位DP` | 已验证：数位 DP 的 windy 数 DFS/递推模板迁入 `/code/dynamic_programming/digit_dp/`，页面引用改为正式模板路径 |
| Wave 4B-1 | `data_structure/segment_tree/update_one`、`update_range` | 已验证：线段树单点加区间和、区间赋值区间和模板迁入 `/code/data-struture/segment_tree/`，页面引用改为正式模板路径 |
| Wave 4B-2 | `data_structure/dance_linking` | 已验证：DLX 精确覆盖模板迁入 `/code/data-struture/dlx/exact_cover.cpp`，页面按教程结构重写，清理旧页面局部模板和生成产物 |
| Wave 4B-3 | `data_structure/segment_tree/scanline`、`sgt_persistent` | 已验证：扫描线面积并、可持久化数组模板迁入 `/code/data-struture/segment_tree/`，两篇页面按教程结构重写，清理旧页面局部模板 |
| Wave 4B-4 | `data_structure/block_data/decompose` | 已验证：分块区间加区间和取模模板迁入 `/code/data-struture/Block/`，页面按教程结构重写，清理旧页面局部模板 |
| Wave 4C-1 | `graph/negative_circle` | 已验证：SPFA 判负环模板迁入 `/code/graph/spfa_negative_cycle.cpp`，页面按教程结构重写，旧空模板删除 |
| Wave 4C-2 | `graph/pseudotree` | 已验证：基环树删叶找环模板迁入 `/code/graph/pseudotree_cycle_nodes.cpp`，页面按教程结构重写，清理旧页面局部模板 |
| Wave 4C-3 | `graph/tarjan/cut`、`graph/tarjan/loop` | 已验证：旧 Tarjan 割点页改为引用 `/code/graph/cut_node.cpp`，找环页改为复用基环树找环模板，清理旧页面局部模板 |
| Wave 4D-1 | `base/bigNumber/add`、`base/zero_number`、`tricks/fraction_class`、`math/集合` | 已验证：高精度加法、正负抵消、分数类、集合最大两数和模板迁入 `book/code/` 正式目录，页面按教程结构重写，清理旧页面局部模板 |
| Wave 4D-2 | `base/binary_search`、`base/number_dis`、`algorithm/discrete/start` | 已验证：二分查找、数字距离模板迁入 `/code/base/`，正式离散化旧入口改为归档页，清理页面局部模板 |
| Wave 4D-3 | `base/double_point/寻找区间和`、`A-B=C数对`、`同向双指针 (滑动窗口)` | 已验证：区间和、最短满足区间、差值数对模板迁入 `/code/base/double_pointer/`，重复滑动窗口旧页改为归档入口，清理旧页面局部模板 |
| Wave 4D-4 | `sort/quicksort` | 已验证：快速排序教程改为正式结构，复用 `/code/base/quick_sort_tranditional.cpp` 和 `/code/base/quick_sort_3way_part.cpp`，旧页改归档，清理页面局部模板和对拍产物 |
| Wave 4D-5 | `utils/template`、`appendix/template`、`utils/random`、`utils/random_graph` | 已验证：通用模板、随机数、随机图生成代码改为引用 `/code/template/` 与 `/code/utils/` 正式模板，清理页面局部 C++ |
| Wave 4D-6 | `data_structure/treap`、`dynamic_programming/knapsack/grouped_knapsack`、`dynamic_programming/number_pyramid`、`graph/topsort`、`string/manacher` | 已验证：新增旋转 Treap 正式教程和 `/code/data-struture/treap.cpp`，清理已由正式模板替代的页面局部 C++，Manacher 正文改为引用 `/code/string/manacher.cpp` |
| Wave 4D-7 | `graph/网络流/dinic` | 已验证：新增 `/code/graph/dinic.cpp`，Dinic 正文改为引用正式模板，清理页面局部 `dinic_cf_template.cpp` |
| Wave 4D-8 | `data_structure/segment_tree/update_one` | 已验证：清理已由正式线段树模板替代的页面局部 `1.cpp`、`2.cpp`；保留 `check1.cpp`、`*gen_dot.cpp` 等对拍/图示生成资源 |
| Wave 4D-9 | `data-structure/fhq-treap`、`tree-algo/hld` | 已验证：补齐 FHQ Treap 与树链剖分正式教程结构，重写 `/code/tree/hld.cpp` 修复旧模板 DFS 与重儿子选择风险 |
| Wave 4D-10 | `graph/save`、`graph/cut/cut_node`、`graph/cut/cut_edge`、`graph/scc`、`graph/bcc` | 已验证：补齐图存储、割点、割边、SCC、BCC 主线教程结构，统一使用正式 `book/code/graph/` 模板 |
| Wave 4D-11 | `graph/二分图/黑白染色`、`graph/netflow/ek` | 已验证：重写二分图黑白染色与 Edmonds-Karp 教程，修正黑白染色模板为独立可运行版本 |
| Wave 4D-12 | `graph/diameter_of_tree`、`data-structure/BIT`、`graph/网络流/dinic` | 已验证：重写树的直径、树状数组、Dinic 主线教程，新增树直径正式模板并修正 BIT 模板为独立可运行版本 |
| Wave 4D-13 | `math/向上取整`、`math/numherTheory/inverse`、`math/numherTheory/Eratosthenes` | 已验证：重写整数向上取整、模逆元、埃氏筛教程，清理问答/博客草稿痕迹并修正数学模板为独立可运行版本 |
| Wave 4D-14 | `data_structure/segment_tree/update_one`、`data_structure/segment_tree/update_range` | 已验证：重写单点修改与区间赋值线段树教程，补齐懒标记讲解，修正两个正式模板为独立可运行版本 |
| Wave 4D-15 | `string/trie`、`string/minimal-string`、`base/tri-search` | 已验证：新增 Trie 与浮点三分正式模板，重写 Trie 与三分教程，英文最小表示法旧入口改为归档页并清理最小表示法模板 |
| Wave 4D-16 | `base/binary_split`、`dynamic_programming/binary_state` | 已验证：重写二分拆分思想与状态压缩 DP 入门教程，新增 bitmask DP 最短 Hamilton 路径正式模板 |
| Wave 4D-17 | `algorithm/dsu_on_tree` | 已验证：重写树上启发式合并教程，新增子树颜色众数统计正式模板 |
| Wave 4D-18 | `graph/bellman-ford`、`graph/floyed` | 已验证：重写 Bellman-Ford 与 Floyd 最短路教程，新增/修正两个正式最短路模板 |
| Wave 4D-19 | `algorithm/binary_search` | 已验证：旧二分查找迁移入口改为归档页，指向正式 `base/binary_search` 教程 |
| Wave 4D-20 | `base/rearrangement_inequality` | 已验证：重写排序不等式教程，新增前缀和的前缀和最小化正式模板 |
| Wave 4D-21 | `data_structure/splay` | 已验证：重写 Splay 伸展树教程，新增普通平衡树六操作正式模板 |
| Wave 4D-22 | `dynamic_programming/slope` | 已验证：重写斜率优化 DP 教程，新增玩具装箱单调队列凸包模板 |
| Wave 4D-23 | `dynamic_programming/多重背包` | 已验证：重写多重背包入口教程，新增二进制拆分正式模板 |
| Wave 4D-24 | `graph/MST` | 已验证：重写最小生成树主教程，修正 Kruskal 模板为独立可运行版本 |
| Wave 4D-25 | `dynamic_programming/sos` | 已验证：重写 SOS DP 教程，新增子集和与超集和正式模板 |
| Wave 4D-26 | `math/numberTheory/prime` | 已验证：重写素数判定与素数筛教程，新增试除法与线性筛正式模板 |
| Wave 4D-27 | `math/numberTheory/欧其里德定理及推论` | 已验证：重写欧几里得定理证明与推论，补齐有限素数集合构造法与应用分类 |
| Wave 4D-28 | `math/numberTheory/remainder` | 已验证：重写余数、同余与取模教程，新增 C++ 规范化取模正式模板 |
| Wave 4D-29 | `dynamic_programming/Quadrangle_Inequality_Optimization` | 已验证：重写四边形不等式优化教程，新增 Knuth 优化石子合并正式模板 |
| Wave 4D-30 | `tricks/quick_swap_two_range` | 已验证：重写三次翻转交换相邻区间技巧，新增正式模板 |
| Wave 4D-31 | `math/logarithm` | 已验证：重写对数基础教程，修正标准记法并补齐算法复杂度推论 |
| Wave 4D-32 | `math/binary` | 已验证：重写二进制与位运算教程，新增常用位运算正式模板并修正最高位操作 |
| Wave 4D-33 | `math/combinatorics/combinatorics` | 已验证：重写排列组合主教程，引用排列、组合和多重集排列正式模板 |
| Wave 4D-34 | `math/combinatorics/rule_of_product` | 已验证：重写乘法原理教程，补齐证明、测试用例和应用分类 |
| Wave 4D-35 | `enumeration_permutaion_combination/permutation/类循环排列` | 已验证：重写类循环排列教程，复用递归多重循环正式模板 |
| Wave 4D-36 | `classical_problem/point2point_problem` | 已验证：重写点对问题模式识别入口，指向正式配对问题与树状数组教程 |
| Wave 4D-37 | `dynamic_programming/number_pyramid/problem` | 已验证：补齐数字三角形练习页，引用正式 DP 模板 |
| Wave 4D-38 | `dynamic_programming/lis/problem` | 已验证：补齐最长上升子序列练习页，引用 O(n^2) 与二分优化模板 |
| Wave 4D-39 | `dynamic_programming/knapsack/exercises/整数划分/problem` | 已验证：重写整数划分练习页，新增完全背包计数正式模板 |
| Wave 4D-40 | `math/set` | 已验证：重写集合基础教程，补齐集合关系、运算、恒等式、证明和应用分类 |
| Wave 4D-41 | `math/combinatorics/模板题目` | 已验证：重写球盒组合模板题，复用第二类 Stirling 数枚举模板验证 25 种方案 |
| Wave 4D-42 | `math/combinatorics/112_combinatorial_problems/1-3-chapter` | 已验证：重写 112 个组合问题 1-3 章整理页，清理待办标记与问答草稿痕迹 |
| Wave 4D-43 | `math/4-6初等数论初步/chapter1` | 已验证：重写整除、公因数与素数基础页，清理未完成定义、内联代码和问答草稿痕迹 |
| Wave 4D-44 | `graph/网络流/费用流` | 已验证：重写最小费用最大流教程，新增 SPFA 残量网络正式模板并清理未完成证明 |
| Wave 4D-45 | `data_structure/segment_tree/sgt_weights` | 已验证：重写权值线段树教程，新增动态开点普通平衡树操作模板 |
| Wave 4D-46 | `enumeration_permutaion_combination/ball_and_box/practice` | 已验证：整理球盒模型练习页，按组合、排列、Stirling 数、隔板法分组 |
| Wave 4D-47 | `preface`、二分教学计划、appendix 支持页 | 已验证：清理已跟踪支持页的待办状态和占位示例 |
| Wave 4D-48 | `data_structure/segment_tree/sgt_first_lt` | 已验证：值域线段树查第一个小于阈值位置模板迁入正式代码目录，页面按教程结构重写，清理旧页面局部对拍代码 |
| Wave 4D-49 | `graph/网络流/dinic/origin`、`ai_fix` | 已验证：旧稿和修改建议改为归档入口，避免 TODO/对话稿继续作为正文页面 |
| Wave 4D-50 | `string/最小表示法/草稿`、`Books/todo_图论与算法_程龚`、配对问题草稿归档 | 已验证：草稿和书单待办页改为归档入口，清理剩余已跟踪 TODO/对话稿噪声 |
| Wave 4D-51 | `math/numberTheory/book_note` | 已验证：数论读书笔记页面局部 Python 演示迁入 `/code/math/numberTheory/` 正式目录，清理页面局部脚本 |
| Wave 4D-52 | `base/bigNumber`、`base/two-pointer`、`dynamic_programming/knapsack/01knapsack` | 已验证：清理未被正文引用、已由正式模板替代的页面局部演示脚本和坏 Haskell 草稿 |
| Wave 4D-53 | `string/进制hash` | 已验证：字符串哈希模板改为独立可运行代码，页面去除内联大段 C++ 并补齐教程结构和应用分类 |
| Wave 4D-54 | `string/manacher` | 已验证：Manacher 页面补齐问题模型、核心直觉和应用分类，统一为当前教程结构 |
| Wave 4D-55 | `string/最小表示法` | 已验证：最小表示法页面补齐问题模型、核心直觉、应用分类和经典例题说明 |
| Wave 4D-56 | `dynamic_programming/数位DP` | 已验证：数位 DP 页面补齐 code_template、教程结构、证明、测试用例、应用分类和经典例题 |
| Wave 4D-57 | `graph/Eulerian-path` | 已验证：新增无向/有向欧拉路正式模板，页面去除不可编译内联代码并补齐教程结构和应用分类 |
| Wave 4D-58 | `data_structure/disjoint_set/种类并查集` | 已验证：补齐种类并查集应用分类，覆盖二分图在线判定、敌对分组和带权/离线约束 |
| Wave 4D-59 | `dynamic_programming/knapsack/exercises/整数划分/problem` | 已验证：补齐整数划分练习页的核心直觉和算法步骤，保持完全背包计数模板引用 |
| Wave 4D-60 | `data_structure/monotonic_queue/滑动窗口`、`最大子序和` | 已验证：两篇单调队列专题页补齐算法步骤、证明、复杂度和应用分类 |
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
