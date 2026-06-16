---
id: "monotonic-stack-luogu5788"
title: "下一个更大元素"
date: 2026-06-16 00:00
toc: true
tags: ["单调栈"]
categories: ["数据结构"]
code_template:
  - title: 下一个更大元素
    desc: "求每个位置右侧第一个更大元素的位置"
    tags: ["单调栈"]
    code: /code/data-struture/monotonic_stack_next_greater.cpp
---

[[TOC]]

## 一句话算法

新来的大元素负责结算它左边所有比它小、且还没找到答案的位置。

## 问题模型

对每个位置 $i$，求右侧第一个满足 $a_j>a_i$ 的位置 $j$。不存在则输出 `0`。

完整讲解见[单调栈](./index.md)。

## 核心直觉

栈中保存还没找到答案的位置。若当前值比栈顶大，当前下标就是栈顶位置的答案；弹出后继续尝试结算新的栈顶。

## 代码实现

@include-code(/code/data-struture/monotonic_stack_next_greater.cpp, cpp)

## 测试用例

输入：

```text
5
1 4 2 3 5
```

输出：

```text
2 5 4 5 0
```

## 经典例题

### 1. [[problem: luogu,P5788]]

标准单调栈模板题。注意输出的是位置，不是值。

### 2. [LeetCode 739](https://leetcode.cn/problems/daily-temperatures/)

求右侧第一个更高温度的距离，结算时输出 `i - top`。
