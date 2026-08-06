---
id: "bit-range-add-range-query"
title: "双树状数组：区间修改与区间查询"
description: 用两个树状数组维护差分及其下标加权值，支持区间加与动态区间和。
date: 2026-08-06 16:36
toc: true
tags: ["数据结构", "树状数组", "差分", "区间修改", "区间查询"]
categories: ["数据结构"]
code_template: [fenwick-range-add-sum]
prerequisites: [bit-range-add-point-query]
---

[[TOC]]

!!! info "学习路线"
[单点修改、区间查询](../index.md) → [区间修改、单点查询](../range-add-point-query/index.md) → **区间修改、区间查询（本篇）**。
!!!

## 前置回顾

上一篇已经得到：

$$
d_i=a_i-a_{i-1},\qquad
a_x=\sum_{i=1}^{x}d_i
$$

区间 $[l,r]$ 增加 $v$，等价于：

$$
d_l\mathrel{+}=v,\qquad d_{r+1}\mathrel{-}=v
$$

一棵树状数组足以恢复一个位置的值。本篇的新问题是：怎样恢复一整个前缀的和。

## 一句话算法

一棵树状数组记录差分，另一棵记录“下标乘差分”，两者组合还原原数组前缀和。

## 问题模型

给定长度为 $n$ 的数组 $a$，支持：

1. 区间修改：给 $a_l,a_{l+1},\ldots,a_r$ 都加上 `value`；
2. 区间查询：求当前 $a_l+a_{l+1}+\cdots+a_r$。

差分树状数组能查询单个 $a_x$。如果为了区间和逐个查询 $a_l,\ldots,a_r$，一次查询最坏仍是 $O(n\log n)$。

我们需要直接计算原数组的前缀和。

## 核心转化

设原数组前缀和为：

$$
P(x)=\sum_{i=1}^{x}a_i
$$

把 $a_i=\sum_{j=1}^{i}d_j$ 代入：

$$
\begin{aligned}
P(x)
&=\sum_{i=1}^{x}\sum_{j=1}^{i}d_j \\
&=\sum_{j=1}^{x}(x-j+1)d_j \\
&=(x+1)\sum_{j=1}^{x}d_j-\sum_{j=1}^{x}j\cdot d_j
\end{aligned}
$$

这个式子只需要两种动态前缀和：

1. `bit_diff` 维护 $d_j$；
2. `bit_weighted` 维护 $j\cdot d_j$。

![](../images/BITn8.png)

![](../images/BITn9.png)

于是：

```cpp
prefix_sum(x) = (x + 1) * sum(bit_diff, x)
              - sum(bit_weighted, x);
```

## 算法步骤

### 同步修改两棵树

对一个差分位置 $p$ 增加 $v$ 时：

```cpp
add(bit_diff, p, v);
add(bit_weighted, p, p * v);
```

因此，对原数组区间 $[l,r]$ 增加 $v$：

```cpp
add(bit_diff, l, v);
add(bit_diff, r + 1, -v);

add(bit_weighted, l, l * v);
add(bit_weighted, r + 1, -(r + 1) * v);
```

### 查询原数组前缀和

分别查询两棵树，再代入推导出的公式：

$$
P(x)=(x+1)\operatorname{sum}_d(x)-\operatorname{sum}_{id}(x)
$$

### 查询任意区间和

最后仍然使用两个前缀相减：

$$
\operatorname{sum}(l,r)=P(r)-P(l-1)
$$

### 小例子

原数组为 $[1,2,3,4,5]$，给 $[2,4]$ 增加 $10$ 后，差分变成：

```text
d = [1, 11, 1, 1, -9]
```

查询 $P(3)$：

$$
\sum_{j=1}^{3}d_j=13,
\qquad
\sum_{j=1}^{3}j d_j=1+22+3=26
$$

$$
P(3)=4\times13-26=26
$$

这与修改后的前三项 $1+12+13=26$ 一致。

## 算法证明

**核心不变量**：`bit_diff` 维护当前差分 $d_i$，`bit_weighted` 维护当前加权差分 $i\cdot d_i$。

1. 一次区间加只改变 $d_l$ 和 $d_{r+1}$；代码在两棵树中同步记录这两个变化，所以不变量保持成立。
2. 两棵树分别正确返回
   $$
   \sum_{i=1}^{x}d_i
   \quad\text{和}\quad
   \sum_{i=1}^{x}i\cdot d_i
   $$
3. 将这两个结果代入恒等式
   $$
   P(x)=(x+1)\sum_{i=1}^{x}d_i-\sum_{i=1}^{x}i\cdot d_i
   $$
   得到正确的原数组前缀和。
4. 两个正确前缀相减，得到正确的区间和。

## 复杂度分析

- 初始化：逐点加入时为 $O(n\log n)$；
- 区间修改：常数次树状数组修改，$O(\log n)$；
- 区间查询：常数次树状数组查询，$O(\log n)$；
- 空间复杂度：两棵长度为 $n$ 的树状数组，仍为 $O(n)$。

## 代码模板

@include-code(/code/data-struture/BIT/bit_区间修改_区间查询.cpp, cpp)

## 完整代码

输入格式与 Luogu P3372 一致：

- `1 l r k`：给区间 $[l,r]$ 增加 $k$；
- `2 l r`：查询区间 $[l,r]$ 的和。

@include-code(./code/p3372_fenwick.cpp, cpp)

## 测试用例

输入：

```text
5 4
1 2 3 4 5
2 1 5
1 2 4 10
2 2 4
2 4 5
```

输出：

```text
15
39
19
```

## 易错点

1. 必须明确第二棵树维护的是 $i\cdot d_i$，因此公式对应 `(x + 1)`；不要与维护 $(i-1)d_i$ 的另一种写法混用。
2. 修改右边界时，下标和权值都要使用 `r + 1`。
3. 两棵树必须同步修改；漏改任何一棵都会破坏前缀和公式。
4. 乘法 `index * value` 容易超过 `int`，应使用 `long long`。
5. 这个方法依赖“区间加”和“区间和”的线性结构，不能直接处理区间赋值、区间乘法或一般区间最值。

## 主练习与下一步

### [[problem: luogu,P3372]] [题解](https://pcs2.roj.ac.cn/problems/luogu/P3372)

P3372 的操作模型正是区间加与区间和。题目名称虽然写着“线段树”，但在只有加法和求和时，双树状数组同样适用；题解中给出了两种做法。

求和主线到这里结束。选学分支：[树状数组维护前缀最值](../prefix-extremum/index.md)。
