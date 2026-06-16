---
id: "diff-pair-count"
title: "A-B=C 数对"
date: 2026-06-16 21:20
toc: true
tags: ["双指针", "数对统计", "排序"]
categories: ["基础算法"]
code_template:
  - title: 差值范围数对暴力
    desc: "排序后二重循环统计差值在 [low, high] 的数对"
    tags: ["暴力", "数对统计"]
    code: /code/base/double_pointer/diff_pair_count_force.cpp
  - title: 差值范围数对双指针
    desc: "固定左端点，用两个右指针维护差值范围"
    tags: ["双指针", "数对统计"]
    code: /code/base/double_pointer/diff_pair_count_range.cpp
---

[[TOC]]

## 一句话算法

排序后固定小的数，满足差值下界和上界的右端点都会向右单调移动。

## 问题模型

给定数组 $a_1,a_2,\dots,a_n$ 和两个整数 `low`、`high`，统计满足：

$$
low \le a_j-a_i \le high,\quad i<j
$$

的数对数量。

当 `low = high = C` 时，就是常见的 `A-B=C` 数对问题。

## 核心直觉

先把数组排序。固定左端点 `i` 后，随着右端点 `j` 向右移动：

$$
a_j-a_i
$$

只会变大。

因此对每个 `i`：

- 第一个满足 `a[j]-a[i] >= low` 的位置记为 `first_ge_low`。
- 第一个满足 `a[j]-a[i] > high` 的位置记为 `first_gt_high`。

那么合法右端点就是半开区间：

```text
[first_ge_low, first_gt_high)
```

贡献为：

$$
first\_gt\_high-first\_ge\_low
$$

当 `i` 右移时，这两个指针不会向左退。

## 算法步骤

1. 将数组排序。
2. 初始化两个右指针：
   - `first_ge_low = 1`
   - `first_gt_high = 1`
3. 枚举左端点 `i = 0..n-1`：
   - 保证两个右指针至少为 `i+1`，避免选同一个元素。
   - 移动 `first_ge_low`，直到差值不小于 `low`。
   - 移动 `first_gt_high`，直到差值大于 `high`。
   - 当前 `i` 的贡献为 `first_gt_high - first_ge_low`。
4. 输出贡献总和。

## 算法证明

**关键不变量：** 对固定的 `i`，`[first_ge_low, first_gt_high)` 正好是所有合法右端点。

1. 排序后，函数 `f(j)=a[j]-a[i]` 随 `j` 单调不降。
2. 所有 `f(j)<low` 的位置一定在左侧，移动 `first_ge_low` 后，它前面都不合法。
3. 所有 `f(j)<=high` 的位置一定在 `first_gt_high` 左侧，移动后它及右侧都不合法。
4. 因此两个边界之间的位置恰好满足 `low <= f(j) <= high`。
5. 当 `i` 变大时，两个边界不会需要向左回退；即使差值变小，也从当前指针继续向右寻找，不会遗漏，因为更靠左的位置已经不满足 `j>i` 或已经被之前左端点分类处理。

每个合法数对都有唯一左端点 `i`，所以按 `i` 累加不重不漏。

## 复杂度分析

- 排序：$O(n\log n)$。
- 双指针扫描：$O(n)$。
- 总时间复杂度：$O(n\log n)$。
- 额外空间复杂度：$O(1)$，不计排序栈和输入数组。

暴力二重循环为 $O(n^2)$。

## 代码实现

### 暴力对照

@include-code(/code/base/double_pointer/diff_pair_count_force.cpp, cpp)

### 双指针优化

@include-code(/code/base/double_pointer/diff_pair_count_range.cpp, cpp)

## 测试用例

输入：

```text
6
1 5 3 4 2 8
2 3
```

输出：

```text
6
```

排序后为 `1 2 3 4 5 8`。差值在 `[2,3]` 的下标对共有 `6` 个。

如果要统计 `A-B=C`，把 `low` 和 `high` 都设为 `C`。

## 应用分类详解

差值数对的本质是“排序后固定一端，另一端的合法范围是连续区间”。

### 一、固定差值数对

**典型模式：** 统计满足 $A-B=C$ 的数对。
**识别信号：** 题面出现“两数之差为 C”“差值等于 K”。
**核心建模：** 令 `low=high=C`，对每个左端点统计合法右端点个数。

| 应用场景 | 经典题目 | 核心思路 |
|---------|---------|---------|
| A-B=C | [[problem: luogu,P1102]] | 排序后二分或双指针统计差值为 C 的数对 |

### 二、差值范围数对

**典型模式：** 统计差值落在一个区间 `[L,R]` 内的数对。
**识别信号：** 题面问“距离不小于/不大于”“差值在范围内”。
**核心建模：** 两个右指针分别维护下界和上界。

| 应用场景 | 经典题目 | 核心思路 |
|---------|---------|---------|
| 距离范围统计 | 本文模型 | 半开区间 `[first_ge_low, first_gt_high)` 给出贡献 |
| 接近目标的数对 | 双指针基础题 | 用有序性排除一整段无效候选 |

### 三、配合二分查找

**典型模式：** 对每个固定端点，用 `lower_bound` 和 `upper_bound` 统计另一个端点数量。
**识别信号：** 写双指针不方便，但数据已排序。
**核心建模：** `lower_bound(a+i+1, low+a[i])` 找下界，`upper_bound(a+i+1, high+a[i])` 找上界。

| 应用场景 | 经典题目 | 核心思路 |
|---------|---------|---------|
| 固定差值计数 | [[problem: luogu,P1102]] | 每个 `A` 二分查 `A-C` 或每个 `B` 查 `B+C` |

## 经典例题

1. [[problem: luogu,P1102]]
   `A-B=C` 模板题。注意重复元素要按下标对计数。

2. 本文差值范围模型
   适合练习两个边界指针如何分别维护下界和上界。

3. Two Sum II
   与差值数对类似，都是排序后利用单调性移动指针。
