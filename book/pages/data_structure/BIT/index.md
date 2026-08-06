---
id: "bit"
title: "树状数组：单点修改与区间查询"
description: 树状数组的基础模型：用二进制长度的区间块支持单点加与动态区间和查询。
date: 2026-08-06 16:36
toc: true
tags: ["数据结构", "树状数组", "前缀和", "区间查询"]
categories: ["数据结构"]
code_template: [fenwick]
---

[[TOC]]

!!! info "学习路线"
树状数组的求和主线是：**单点修改、区间查询（本篇）** → [区间修改、单点查询](./range-add-point-query/index.md) → [区间修改、区间查询](./range-add-range-query/index.md)。[前缀最值](./prefix-extremum/index.md) 是选学分支。
!!!

## 一句话算法

树状数组把前缀拆成少量二进制长度的块，修改和查询都只在这些块之间跳转。

## 问题模型

给定长度为 $n$ 的数组 $a$，操作会反复交替出现：

1. 单点修改：`a[x] += value`；
2. 区间查询：求 $a_l+a_{l+1}+\cdots+a_r$。

静态前缀和可以 $O(1)$ 查询区间和，但修改一个位置后，后面的所有前缀和都要更新，单次修改最坏是 $O(n)$。

树状数组把两种操作都降到 $O(\log n)$。

## 核心直觉

### 每个节点维护哪一段

定义：

```cpp
lowbit(x) = x & -x
```

树状数组节点 `tree[i]` 维护：

$$
a_{i-\operatorname{lowbit}(i)+1}+\cdots+a_i
$$

也就是以 $i$ 为右端点、长度为 $\operatorname{lowbit}(i)$ 的区间。

例如：

| $i$ | $\operatorname{lowbit}(i)$ | `tree[i]` 维护的区间 |
|---:|---:|---:|
| 3 | 1 | $[3,3]$ |
| 6 | 2 | $[5,6]$ |
| 8 | 8 | $[1,8]$ |
| 12 | 4 | $[9,12]$ |

在 $1\ldots 8$ 中，这些区间互相嵌套：

```text
index:   1  2  3  4  5  6  7  8
node 1: [1]
node 2: [1--2]
node 3:       [3]
node 4: [1--------4]
node 5:             [5]
node 6:             [5--6]
node 7:                   [7]
node 8: [1--------------------8]
```

![](./images/Binary_Indexed_Tree.png)

### `lowbit` 为什么得到块长

`-x` 使用补码表示。`x & -x` 会清掉最低位 `1` 以外的所有位，只留下这个最低位 `1` 所代表的值。

例如：

```text
 12 = 00001100
-12 = 11110100
 &  = 00000100 = 4
```

所以 `lowbit(12)=4`，节点 `12` 的块长就是 $4$。

## 算法步骤

### 单点修改

修改 `a[pos] += value` 时，所有覆盖 `pos` 的块都要增加 `value`：

```cpp
for (int i = pos; i <= n; i += lowbit(i)) {
    tree[i] += value;
}
```

例如修改 `a[3]`，更新路径是：

```text
3 --+1--> 4 --+4--> 8 --+8--> 16（越界停止）
```

`3、4、8` 对应的块都覆盖位置 $3$。每次执行 `i += lowbit(i)`，都会跳到下一个覆盖原位置的更大块。

### 前缀查询

查询前缀 $a_1+\cdots+a_{pos}$ 时，每次取走一个以当前位置结尾的完整块：

```cpp
answer = 0;
for (int i = pos; i > 0; i -= lowbit(i)) {
    answer += tree[i];
}
```

例如查询前缀 $[1,7]$：

```text
7 --1--> 6 --2--> 4 --4--> 0

[7,7] + [5,6] + [1,4] = [1,7]
```

这些块互不重叠，刚好拼成整个前缀。

### 区间查询

区间和仍然使用两个前缀相减：

$$
\operatorname{sum}(l,r)
=\operatorname{prefix}(r)-\operatorname{prefix}(l-1)
$$

## 算法证明

**核心不变量**：`tree[i]` 始终等于区间

$$
[i-\operatorname{lowbit}(i)+1,i]
$$

内所有元素的和。

1. 单点修改时，`i += lowbit(i)` 恰好枚举所有覆盖修改位置的节点，因此所有受影响的块都增加了 `value`，其他块不变。
2. 前缀查询时，`i -= lowbit(i)` 每次取走当前前缀最右侧的完整块。取出的块两两不交，并且最终覆盖整个 $[1,pos]$。
3. 因为两个前缀和都正确，所以它们相减后得到 $[l,r]$ 的区间和。

因此修改与查询都保持正确。

## 复杂度分析

下标每次跳转都会消去或进位一个二进制低位，因此一次循环最多执行 $O(\log n)$ 次。

- 初始化：逐点加入时为 $O(n\log n)$；
- 单点修改：$O(\log n)$；
- 前缀查询、区间查询：$O(\log n)$；
- 空间复杂度：$O(n)$。

## 代码模板

下面的模板只保留可复用的 `add`、`prefix_sum` 和 `range_sum` 接口：

@include-code(/code/data-struture/BIT/bit.cpp, cpp)

## 完整代码

输入格式与 Luogu P3374 一致：

- `1 x k`：执行 `a[x] += k`；
- `2 l r`：查询 $[l,r]$ 的区间和。

@include-code(./code/p3374.cpp, cpp)

## 测试用例

输入：

```text
5 5
1 2 3 4 5
2 1 5
1 3 2
2 2 4
1 5 -1
2 4 5
```

输出：

```text
15
11
8
```

第二次查询时，数组已经变成 $[1,2,5,4,5]$，所以 $a_2+a_3+a_4=11$。

## 易错点

1. 树状数组必须使用从 $1$ 开始的下标；`lowbit(0)=0` 会让修改循环无法前进。
2. 区间 $[l,r]$ 要写成 `prefix_sum(r) - prefix_sum(l - 1)`。
3. 元素和可能超过 `int`，竞赛中通常使用 `long long`。
4. `add(pos, value)` 表示增加 `value`，不是把 `a[pos]` 赋值为 `value`。

## 主练习与下一步

### [[problem: luogu,P3374]] [题解](https://pcs2.roj.ac.cn/problems/luogu/P3374)

这道题与本篇接口完全一致。完成后应能独立写出三个操作：单点加、前缀和、区间和。

下一篇：[树状数组与差分：区间修改与单点查询](./range-add-point-query/index.md)。
