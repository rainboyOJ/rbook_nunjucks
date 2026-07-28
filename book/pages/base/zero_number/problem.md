---
id: base-zero-number-problem
title: problem
description: 数列归零问题的题目描述：通过加减操作使所有数变成 0，求最少操作次数。
tags: ["贪心", "等效模型", "基础算法"]
---


### 问题描述

有n个数,``a_1,a_2,a_3,\cdots,a_n``,,现在有以下操作

1. 选取任意一个数,``a_i``,加上或减去``1``
2. 选取任意两个数,``a_i,a_j,i \neq j``,其中一个加1,另一个减1

问题,最少操作多少次

1. 所有的数变成0
2. 除了第一个数外,其它所有的数变成0


## 输入样例


```
@include-code(./in.txt, text)
```

## 输出样例

```
@include-code(./out.txt, text)
```

## 数据范围

``n \le 10^5``,``|a_i| \le 10^5``
