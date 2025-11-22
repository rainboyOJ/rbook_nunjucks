---
title: "最小表示法"
date: 2025-11-22 19:43
toc: true
tags: ["字符串","集合最值"]
categories: ["字符串"]
code_template:
  - title: 最小表示法
    tags: ["字符串","集合最值"]
    code: /code/string/minimal-string.cpp
---

[[TOC]]


> 环形字符串最小表示法

## 循环同构

定义

## 暴力

暴力解法的思想很简单：枚举所有可能的起始位置（从 0 到 n-1），然后两两比较，找出字典序最小的那个作为答案。

本质思想$\min B =\min( \min (B \setminus c) ,\{c\} )$ 

```cpp
#include <iostream>
#include <string>
#include <vector>
#include <algorithm>

// 暴力解法 (for-loop version)
// s: 传入的字符串
// 返回值: 最小表示法的起始位置（0-indexed）
int get_min_representation_brute_force(const std::string& s) {
    int n = s.length();
    if (n == 0) return 0;

    int min_pos = 0; // 假设最小表示法的起始位置是0

    // 挑战者,遍历所有可能的起始位置 i (从1到n-1)
    // 这里用到的是集合思想: min_pos 表示 $s1,..si-1$字符串的最小值 
    for (int i = 1; i < n; ++i) {
        // 将从 i 开始的字符串与当前已知的最小字符串（从 min_pos 开始）进行比较
        for (int k = 0; k < n; ++k) {
            char char_i = s[(i + k) % n];
            char char_min = s[(min_pos + k) % n];
            
            if (char_i < char_min) {
                // 找到了一个更小的表示，更新 min_pos
                min_pos = i;
                break; // 结束与旧 min_pos 的比较，i+1 继续
            }
            if (char_i > char_min) {
                // 当前从 i 开始的字符串更大，不是最小表示，直接跳出比较
                break;
            }
            // 如果字符相等，继续比较下一个字符
        }
    }
    return min_pos;
}
```

显然这个代码在`aaa...aab`下面比较次数比较多,退化成$O(n^2)$

## 优化暴力

核心思想: 和KMP思想一样,在于前缀相同时的处理:**排除不可能答案**

- $S_i$ 表示以位置$i$起始的字符串

如果$S_i,S_j$的前 $k$ 个位置字符一样

$$
S(i,i+k-1) = S(j,j+k-1)
$$

但是第$k+1$个位置不一样,假定$S[i+k] < S[j+k]$

则$S_{j+p}, 0 \leqslant p \leqslant k$,都不可能是答案. 因为$S_{i+p} < S_{j+p}$一定.

那么此时排除不可能答案: $j \to j = j+k+1$

## 代码实现

下面是根据上述思想实现的C++代码。函数 `get_min_` 接收一个字符串，返回其最小表示法的起始下标。


@include-code(/code/string/minimal-string.cpp, cpp)


## 参考

- https://oi-wiki.org/string/minimal-string/


## 题目

- luogu 10476
- acwing 136
- [luogu P13270 【模板】最小表示法](https://www.luogu.com.cn/problem/P13270)

-这个题目告诉我们,不是循环的串,也可以求最小自串 https://leetcode.cn/problems/last-substring-in-lexicographical-order/description/