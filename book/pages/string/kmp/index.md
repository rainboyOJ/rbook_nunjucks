---
id: "kmp"
title: "KMP 字符串匹配"
date: 2026-06-16 00:00
toc: true
tags: ["字符串", "KMP", "模式匹配"]
categories: ["字符串"]
code_template:
  - title: KMP 字符串匹配
    desc: "用前缀函数在线性时间找出模式串所有出现位置"
    tags: ["字符串", "KMP"]
    code: /code/string/kmp.cpp
---

[[TOC]]

## 一句话算法

KMP 在失配时不回头重比主串，而是利用模式串自己的相同前后缀，直接跳到下一个可能匹配长度。

## 问题模型

给定：

- 主串 `text`，长度为 $n$；
- 模式串 `pattern`，长度为 $m$。

要求找出 `pattern` 在 `text` 中出现的所有起始位置。

朴素匹配的最坏复杂度是 $O(nm)$。KMP 通过预处理模式串，把匹配过程优化到 $O(n+m)$。

## 核心直觉

当已经匹配了 `j` 个字符，却在下一个字符失配时：

```text
text:       ... a b a b a c ...
pattern:       a b a b a d
                   已匹配 5 个，下一位失配
```

我们已经知道 `pattern[0..j-1]` 和主串刚刚这一段完全相同。此时不应该把主串指针退回去重比。

真正有用的问题是：

> 在已经匹配的这一段里，最长的“既是前缀又是后缀”的长度是多少？

如果这个长度是 `k`，那么下一次可以保留前 `k` 个字符的匹配状态，继续尝试：

```text
pattern 前缀: a b a
pattern 后缀:     a b a
```

这就是前缀函数 `pi` 保存的信息。

!!! definition "前缀函数"
`pi[i]` 表示 `pattern[0..i]` 的最长相等真前缀和真后缀的长度。
!!!

例如：

```text
pattern: a b a b a b a c
index:   0 1 2 3 4 5 6 7
pi:      0 0 1 2 3 4 5 0
```

## 算法步骤

### 1. 构建前缀函数

对每个位置 `i`，维护 `j` 表示当前能继承的最长前后缀长度。

1. 初始 `pi[0] = 0`。
2. 枚举 `i = 1..m-1`。
3. 若 `pattern[i] != pattern[j]`，令 `j = pi[j-1]`，尝试更短的候选前缀。
4. 若 `pattern[i] == pattern[j]`，说明当前候选可以扩展，执行 `j++`。
5. 令 `pi[i] = j`。

### 2. 扫描主串

1. 用 `j` 表示当前已经匹配的模式串长度。
2. 枚举主串位置 `i`。
3. 若 `text[i] != pattern[j]`，令 `j = pi[j-1]` 回退到下一个候选长度。
4. 若 `text[i] == pattern[j]`，执行 `j++`。
5. 若 `j == m`，说明找到一次匹配，起点为 `i-m+1`。
6. 为了继续找重叠匹配，令 `j = pi[j-1]`。

## 算法证明

**核心不变量**：扫描到主串位置 `i` 后，`j` 始终表示“`text` 的当前后缀”和 `pattern` 前缀能匹配的最长长度。

### 1. 为什么失配时可以跳到 `pi[j-1]`

失配前已经有：

$$
text[i-j,i-1] = pattern[0,j-1]
$$

如果下一位失配，长度为 `j` 的匹配不能继续。新的候选必须同时满足：

1. 它是 `pattern[0,j-1]` 的后缀，因为它要贴住当前主串后缀；
2. 它是 `pattern` 的前缀，因为下一步仍要从模式串前缀继续匹配。

所以新候选长度只能是 `pattern[0,j-1]` 的某个相等前后缀长度。最长的那个正是：

$$
pi[j-1]
$$

因此 `j = pi[j-1]` 不会漏掉可能答案。

### 2. 为什么复杂度是线性的

`i` 每轮只增加一次。`j` 只会因为匹配而增加，因为失配而沿着 `pi` 回退。

每次回退都会让 `j` 变小，而 `j` 总共最多增加 $n$ 次，所以总回退次数也是 $O(n)$。

构建 `pi` 的过程同理为 $O(m)$。

因此 KMP 的总复杂度是 $O(n+m)$。

## 复杂度分析

- 构建前缀函数：$O(m)$。
- 扫描主串：$O(n)$。
- 总时间复杂度：$O(n+m)$。
- 空间复杂度：$O(m)$。

## 代码实现

@include-code(/code/string/kmp.cpp, cpp)

## 测试用例

输入：

```text
abababaca aba
```

输出：

```text
0 2 4
```

解释：KMP 会找到包括重叠情况在内的所有出现位置。

## 应用分类详解

KMP 的本质是：在一个串中维护“当前后缀等于模式串前缀”的最长长度。只要题目需要在线性时间处理字符串匹配、边界、循环节，就应该想到 KMP 或前缀函数。

### 一、单模式串匹配

**典型模式：** 给一个文本串和一个模式串，找所有出现位置。

**识别信号：** 出现“模式串在文本串中出现几次”“所有起始位置”“允许重叠”。

**核心建模：** 用 `pi` 记录模式串失配后应该保留的匹配长度。

| 应用场景 | 经典题目 | 核心思路 |
|---|---|---|
| KMP 模板 | [[problem: luogu,P3375]] | 输出所有匹配位置和 next 数组 |
| 第一次出现位置 | [LeetCode 28](https://leetcode.cn/problems/find-the-index-of-the-first-occurrence-in-a-string/) | KMP 可保证线性复杂度 |

### 二、字符串 border 问题

**典型模式：** 研究一个字符串的相等前缀和后缀。

**识别信号：** 出现“既是前缀又是后缀”“最长公共前后缀”“border”。

**核心建模：** `pi[n-1]` 就是整个字符串的最长 border 长度，继续沿 `pi[x-1]` 可以枚举所有 border。

| 应用场景 | 经典题目 | 核心思路 |
|---|---|---|
| 枚举所有 border | [[problem: luogu,P3435]] | 沿前缀函数不断回退 |
| 前后缀统计 | [Codeforces 432D](https://codeforces.com/problemset/problem/432/D) | border 枚举加出现次数统计 |

### 三、循环节与周期

**典型模式：** 判断一个字符串是否由某个短串重复得到。

**识别信号：** 出现“最小循环节”“周期”“重复字符串”。

**核心建模：** 设 $L = n - pi[n-1]$。如果 $n \bmod L = 0$，则 $L$ 是最小循环节长度。

| 应用场景 | 经典题目 | 核心思路 |
|---|---|---|
| 最小循环节 | [POJ 1961](http://poj.org/problem?id=1961) | 用 `n - pi[n-1]` 判断周期 |
| 重复子串模式 | [LeetCode 459](https://leetcode.cn/problems/repeated-substring-pattern/) | 前缀函数判断是否存在整周期 |

### 四、作为自动机和多算法前置

**典型模式：** 需要理解 AC 自动机、字符串自动机、最小表示法中的“失配跳转”。

**识别信号：** 出现“fail 指针”“失配边”“排除不可能答案”。

**核心建模：** KMP 的 `pi` 是单模式串上的失配跳转，AC 自动机把这个思想推广到 Trie 上。

| 应用场景 | 经典题目 | 核心思路 |
|---|---|---|
| AC 自动机入门 | [[problem: luogu,P3808]] | 多模式串上的 fail 指针 |
| 最小表示法理解 | 本书最小表示法章节 | 失配后批量排除候选起点 |

## 经典例题

### 1. [[problem: luogu,P3375]]

KMP 模板题。要求输出匹配位置和 `next` 数组，适合检查前缀函数实现是否正确。

### 2. [Codeforces 432D](https://codeforces.com/problemset/problem/432/D)

要求找出所有既是前缀又是后缀的字符串，并统计出现次数。核心是沿 `pi` 链枚举 border。

### 3. [LeetCode 459](https://leetcode.cn/problems/repeated-substring-pattern/)

判断字符串能否由某个子串重复得到。用 `n - pi[n-1]` 得到候选周期长度。

## 参考

- 旧版文章：`Rbook_ejs_old/book/string/kmp/index.md`
- [Knuth-Morris-Pratt algorithm - Wikipedia](https://en.wikipedia.org/wiki/Knuth%E2%80%93Morris%E2%80%93Pratt_algorithm)
