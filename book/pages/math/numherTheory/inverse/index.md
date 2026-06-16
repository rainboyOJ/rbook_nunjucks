---
id: "inverse"
title: "模逆元"
date: 2026-06-16 00:00
toc: true
tags: ["数学", "数论", "逆元"]
categories: ["数学"]
code_template:
  - title: 扩展欧几里得求逆元
    desc: "求单个数在任意互质模数下的逆元"
    tags: ["数论", "逆元", "exgcd"]
    code: /code/math/inv_exgcd.cpp
  - title: 线性求逆元
    desc: "在线性时间内求 1..n 在质数模数下的逆元"
    tags: ["数论", "逆元", "线性递推"]
    code: /code/math/line_inv.cpp
---

[[TOC]]

## 一句话算法

模逆元就是模意义下的“倒数”，除以一个数可以改成乘它的逆元。

## 问题模型

给定整数 `a` 和模数 `m`，若存在整数 `x` 满足：

$$
a \times x \equiv 1 \pmod m
$$

则称 `x` 是 `a` 在模 `m` 意义下的逆元，记作 $a^{-1}$。

逆元存在的充要条件是：

$$
\gcd(a,m)=1
$$

## 核心直觉

在普通除法中：

$$
\frac{A}{b}=A \times \frac{1}{b}
$$

在模运算中不能直接除法，但如果能找到一个数 $b^{-1}$，使得：

$$
b \times b^{-1} \equiv 1 \pmod m
$$

那么就可以把除法改成乘法：

$$
\frac{A}{b} \equiv A \times b^{-1} \pmod m
$$

## 算法步骤

### 扩展欧几里得求单个逆元

要求 `a` 在模 `m` 下的逆元，就是解：

$$
a x \equiv 1 \pmod m
$$

等价于：

$$
a x + m y = 1
$$

这正是扩展欧几里得能求的形式。

步骤：

1. 用 `exgcd(a, m, x, y)` 求出一组解。
2. 若 `gcd(a, m) != 1`，逆元不存在。
3. 否则 `x` 就是一个逆元。
4. 用 `(x % m + m) % m` 调整到 `[0, m-1]`。

### 线性求 1..n 的逆元

当模数 `p` 是质数，并且要计算 `1..n` 所有逆元时，可以线性递推：

$$
inv[i] = (p-\lfloor p/i \rfloor) \times inv[p\bmod i] \bmod p
$$

初始条件：

$$
inv[1] = 1
$$

## 算法证明

### 为什么除法能变成乘逆元

设：

$$
x \equiv \frac{A}{b} \pmod m
$$

这表示：

$$
b x \equiv A \pmod m
$$

两边同乘 $b^{-1}$：

$$
b^{-1} b x \equiv A b^{-1} \pmod m
$$

因为 $b^{-1}b \equiv 1 \pmod m$，所以：

$$
x \equiv A b^{-1} \pmod m
$$

### 线性递推公式

令：

$$
p = \left\lfloor \frac{p}{i} \right\rfloor i + (p \bmod i)
$$

在模 `p` 意义下：

$$
\left\lfloor \frac{p}{i} \right\rfloor i + (p \bmod i) \equiv 0
$$

两边同乘 $i^{-1}(p\bmod i)^{-1}$：

$$
\left\lfloor \frac{p}{i} \right\rfloor (p\bmod i)^{-1} + i^{-1} \equiv 0
$$

移项得：

$$
i^{-1} \equiv -\left\lfloor \frac{p}{i} \right\rfloor (p\bmod i)^{-1}
$$

写成非负取模形式就是：

$$
inv[i] = (p-\lfloor p/i \rfloor) \times inv[p\bmod i] \bmod p
$$

## 复杂度分析

- 扩展欧几里得求单个逆元：$O(\log m)$。
- 线性求 `1..n` 逆元：$O(n)$。
- 空间复杂度：扩展欧几里得为 $O(1)$，线性递推为 $O(n)$。

## 代码实现

### 扩展欧几里得求逆元

@include-code(/code/math/inv_exgcd.cpp, cpp)

### 线性求逆元

@include-code(/code/math/line_inv.cpp, cpp)

## 测试用例

扩展欧几里得求逆元：

```text
3 11
```

输出：

```text
4
```

因为 $3 \times 4 \equiv 1 \pmod {11}$。

线性求逆元：

```text
5 7
```

输出：

```text
1
4
5
2
3
```

## 应用分类详解

模逆元的本质是把模意义下的除法转成乘法。凡是题目要求“取模后除以某个数”，都要想到逆元。

### 一、单次模除法

**典型模式：** 求 `a / b mod m`。

**识别信号：** 出现“答案对 m 取模”，公式中又有除法。

**核心建模：** 先求 `b` 的逆元，再计算 `a * inv(b) % m`。

| 应用场景 | 经典题目 | 核心思路 |
|---|---|---|
| 单个逆元 | [[problem: 51Node,1256]] | exgcd 求解 `ax + my = 1` |
| 逆元模板 | [[problem: luogu,P3811]] | 线性递推求 `1..n` 所有逆元 |

### 二、组合数取模

**典型模式：** 求 $C(n,m) \bmod p$。

**识别信号：** 公式中有阶乘相除：$\frac{n!}{m!(n-m)!}$。

**核心建模：** 预处理阶乘和阶乘逆元，把除法改成乘逆元。

### 三、概率与期望取模

**典型模式：** 概率是分数，但题目要求模意义输出。

**识别信号：** 出现“若答案为 P/Q，输出 P * Q^{-1} mod M”。

**核心建模：** 分母用逆元消掉。

## 经典例题

### 1. [[problem: luogu,P3811]]

乘法逆元模板题。要求在线性时间内输出 `1..n` 的逆元。

### 2. [[problem: 51Node,1256]]

单个逆元题。适合练习扩展欧几里得求逆元。

### 3. 组合数取模模板题

重点是把除以阶乘转成乘阶乘逆元。

## 参考

- 本书扩展欧几里得章节：`math/numberTheory/exgcd/index.md`
