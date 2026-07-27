# code.yaml 代码模板复核报告

分析依据：读取每个条目对应的源码，并对照当前 `book/code.yaml` 路径，判断它更接近“可直接复用的代码模板”，还是“教学/演示/具体问题程序”。

模板判断标准：

- 优先保留抽象函数、结构体、类型定义、通用算法实现。
- 不应强依赖某个固定输入输出格式。
- 可以有少量演示代码，但若文件主体是 `main` + 固定 IO，就不应作为模板注册项。

---

## ❌ 不是代码模板，建议迁出 book/code

| id | 原因 |
|---|---|
| `binary-search-linear` | 完整问题求解，O(n)线性扫描+查询输出，无抽象封装 |

## ✅ 已修正为纯模板

这些条目原先都带有 `main` 和固定输入输出。当前已改成无 IO 依赖的模板函数，可继续作为 `code_template` 使用。

| id | 现状 |
|---|---|
| `combination` | `enumerate_combinations(a, m, emit)`，回调式组合枚举 |
| `enumerate-dynamic-loop` | `enumerate_dynamic_loop(n, m, emit)`，回调式多重循环枚举 |
| `multiset-permutation` | `count_distinct_permutations(a)` + `enumerate_multiset_permutations(a, emit)` |


---

## ⚠️ 有可复用核心，但不是干净的纯模板

这些条目包含可复用的算法核心，但部分文件同时带有 `main`、固定输入输出或测试输出。它们可以暂时保留为模板入口，但长期建议拆成“纯模板文件 + 文章演示代码”。

| id | 模板内容 |
|---|---|
| `difference-1d` | 有 `struct DifferenceArray`，但文件同时包含固定 IO 的 `main` |
| `difference-2d` | 有 `struct Difference2D`，但文件同时包含固定 IO 的 `main` |
| `discretize-stl` | 有 `struct Discrete`，但文件同时包含演示 IO 的 `main` |
| `next-permutation` | 有可复用函数 `next_permutation_manual`，但文件同时包含排列输出程序 |
| `prefix-sum` | 有 `struct PrefixSum`，但文件同时包含固定查询 IO 的 `main` |
| `bignum-add` | 有可复用函数 `add_positive_integer`，但文件同时包含读入两个字符串并输出的 `main` |
| `rbtree-hs` | 有红黑树核心实现，但文件后半部分包含大量测试函数和 `main` |

---

## ✅ 更接近模板片段，但需要补完整性

| id | 说明 |
|---|---|
| `discretize-manual` | 主要是 `struct DiscreteManual`，没有 `main`，但缺少 include/import，且末尾带全局对象 `disc` |
| `all-substrings-hs` | 主要是纯函数 `allSubstrings`，但依赖 `tails` / `inits`，文件内缺少 `Data.List` import |

---

## 已复核的路径状态

`bignum-add` 当前在 `book/code.yaml` 中的路径是 `base/big_number/add_string.cpp`，实际文件存在；不存在 `base/bignum/add_string.cpp` 路径错误。
