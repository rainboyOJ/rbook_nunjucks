---
id: "crt"
title: "中国剩余定理 (Chinese Remainder Theorem)"
date: 2025-12-15 08:44
toc: true
tags: ["数论", "同余", "模运算"]
categories: ["数学"]
---

[[TOC]]

## 摘要 (Abstract)

中国剩余定理 (Chinese Remainder Theorem, CRT) 是数论中的一个核心定理，用于求解一组线性同余方程。当模数两两互质时，CRT 提供了一种高效的构造性方法，能够找到满足所有方程的唯一解（在模所有模数之积的意义下）。该定理不仅在纯数学领域有重要意义，在计算机科学、密码学和工程领域也有着广泛应用。

## 背景与动机 (Motivation)

中国剩余定理最早见于中国南北朝时期的数学著作《孙子算经》中，被称为“孙子问题”或“物不知数”问题。原文如下：

> “今有物不知其数，三三数之剩二，五五数之剩三，七七数之剩二，问物几何？”

这个问题用现代数学语言描述，就是求解一个线性同余方程组。CRT 不仅是一个古老的智力谜题，它在现代技术中也扮演着关键角色。例如，在密码学（如 RSA 算法）中，CRT 可以加速模幂运算；在计算机科学中，它可以用于处理大数运算，通过将一个大数上的计算分解到多个较小的模数上并行处理，然后用 CRT 合并结果。

## 问题定义 (Problem Definition)

给定一组 $n$ 个线性同余方程：
$$
\begin{cases}
x \equiv a_1 \pmod{m_1} \\
x \equiv a_2 \pmod{m_2} \\
\vdots \\
x \equiv a_n \pmod{m_n}
\end{cases}
$$
其中 $m_1, m_2, \dots, m_n$ 是两两互质的正整数。我们的目标是找到一个整数 $x$，满足所有这些方程。

## 一句话算法

**模数累乘为总模，部分模积求逆元，各项累加构造解。**

这句口诀概括了 CRT 的核心构造步骤：首先计算所有模数的乘积 $M$，然后对每个方程，计算 $M$ 除以当前模数 $m_i$ 的“部分模积” $M_i$，并求出 $M_i$ 在模 $m_i$ 下的逆元，最后将各项结果加权相加，得到最终解。

## 关键思路 (Key Idea)

CRT 的精髓在于“分解与合并”。我们试图将求解一个复杂的联立方程组问题，分解为 $n$ 个独立的小问题，然后将这些小问题的解巧妙地组合起来。

具体来说，我们的目标是构造一个解 $x$。我们可以尝试构造 $n$ 个特殊的数 $e_1, e_2, \dots, e_n$，使得：
- $e_i \equiv 1 \pmod{m_i}$
- $e_i \equiv 0 \pmod{m_j}$ (对于所有 $j \neq i$)

如果能找到这样的 $e_i$，那么最终的解就可以被构造为：
$$x = a_1 e_1 + a_2 e_2 + \dots + a_n e_n$$
为什么这个构造是正确的？当我们验证 $x \pmod{m_i}$ 时，所有 $j \neq i$ 的项 $a_j e_j$ 都变成了 $0$，只剩下 $a_i e_i$。因为 $e_i \equiv 1 \pmod{m_i}$，所以 $a_i e_i \equiv a_i \pmod{m_i}$。这样，构造出的 $x$ 就满足了第 $i$ 个方程。由于这对所有 $i$ 都成立，所以它就是整个方程组的解。

那么，如何构造 $e_i$ 呢？
1.  为了满足 $e_i \equiv 0 \pmod{m_j}$ (对于 $j \neq i$)， $e_i$ 必须是所有 $m_j$ 的公倍数。最简单的方法是让 $e_i$ 包含 $\prod_{j \neq i} m_j$ 这个因子。我们令 $M_i = \prod_{j \neq i} m_j = (\prod_{k=1}^n m_k) / m_i$。
2.  现在我们需要满足 $e_i \equiv 1 \pmod{m_i}$。我们已经有了 $M_i$，需要再乘以一个数 $t_i$，使得 $M_i \cdot t_i \equiv 1 \pmod{m_i}$。这个 $t_i$ 正是 $M_i$ 在模 $m_i$ 意义下的乘法逆元。

因此， $e_i = M_i \cdot (M_i^{-1} \pmod{m_i})$。最终解就是 $x = \sum_{i=1}^n a_i M_i t_i$。

## 算法步骤 (Algorithm Steps)

1.  **计算总模**：计算所有模数的乘积 $M = \prod_{i=1}^n m_i$。
2.  **遍历每个方程**: 对于第 $i = 1, \dots, n$ 个方程：
    a. **计算部分模积**: $M_i = M / m_i$。
    b. **求解逆元**: 使用扩展欧几里得算法 (Extended Euclidean Algorithm) 求解 $M_i$ 在模 $m_i$ 下的乘法逆元 $t_i$，使得 $M_i \cdot t_i \equiv 1 \pmod{m_i}$。
3.  **构造解**: 根据公式 $x = \sum_{i=1}^n a_i M_i t_i$ 计算出一个特解。
4.  **求最小正整数解**: 最终解是 $x \pmod M$。为了确保是最小正整数解，可以写成 $(x \pmod M + M) \pmod M$。

## 算法证明

我们来证明构造出的解 $x_0 = \sum_{j=1}^n a_j M_j t_j$ 是方程组的一个解。
我们需要验证对于任意 $k \in \{1, \dots, n\}$，都有 $x_0 \equiv a_k \pmod{m_k}$。

$$
x_0 \pmod{m_k} = \left( \sum_{j=1}^n a_j M_j t_j \right) \pmod{m_k}
$$

我们将求和拆开：
- 当 $j \neq k$ 时， $M_j = M/m_j$ 的定义中包含了因子 $m_k$ (因为模数两两互质)。因此，$M_j \equiv 0 \pmod{m_k}$。所以 $a_j M_j t_j \equiv 0 \pmod{m_k}$。
- 当 $j = k$ 时，该项为 $a_k M_k t_k$。根据 $t_k$ 的定义，我们有 $M_k t_k \equiv 1 \pmod{m_k}$。

因此，原求和式在模 $m_k$ 意义下，只有第 $k$ 项不为零：
$$
x_0 \equiv 0 + \dots + 0 + a_k (M_k t_k) + 0 + \dots + 0 \pmod{m_k}
$$
$$
x_0 \equiv a_k \cdot 1 \pmod{m_k}
$$
$$
x_0 \equiv a_k \pmod{m_k}
$$
此式对所有 $k=1, \dots, n$ 均成立，故 $x_0$ 是方程组的一个解。所有通解可以表示为 $x_0 + kM$ ($k$ 为任意整数)，最小非负整数解即为 $x_0 \pmod M$。

## 复杂度分析

- **时间复杂度**: $O(\sum_{i=1}^n \log m_i)$。
  - 计算总模 $M$ 需要 $O(n)$ 次大数乘法。
  - 循环 $n$ 次，每次循环中：
    - 计算 $M_i$ 需要一次大数除法。
    - 求解逆元 $t_i$ 使用扩展欧几里得算法，复杂度为 $O(\log m_i)$。
  - 最后累加构造解需要 $O(n)$ 次大数乘法和加法。
  - 算法的主要开销在于求解 $n$ 个逆元，因此总时间复杂度为 $O(\sum_{i=1}^n \log m_i)$。

- **空间复杂度**: $O(1)$ (不考虑存储输入数组和和大数运算的开销)。

## 代码实现

```cpp
#include <iostream>
#include <vector>

// 使用 long long 防止中间计算溢出
using ll = long long;

// 扩展欧几里得算法，求解 ax + by = gcd(a, b)
// 返回 gcd(a, b)，并将 x, y 的解存入引用
ll extended_gcd(ll a, ll b, ll &x, ll &y) {
    if (a == 0) {
        x = 0;
        y = 1;
        return b;
    }
    ll x1, y1;
    ll gcd = extended_gcd(b % a, a, x1, y1);
    x = y1 - (b / a) * x1;
    y = x1;
    return gcd;
}

// 求解 a 在模 m 下的乘法逆元
// 要求 a 和 m 互质
ll mod_inverse(ll a, ll m) {
    ll x, y;
    ll g = extended_gcd(a, m, x, y);
    if (g != 1) {
        // 逆元不存在
        return -1;
    }
    // 保证返回的是正数
    return (x % m + m) % m;
}

// 中国剩余定理
// a: 余数数组 (a_1, a_2, ...)
// m: 模数数组 (m_1, m_2, ...)
ll chinese_remainder_theorem(const std::vector<ll>& a, const std::vector<ll>& m) {
    int n = a.size();
    ll M = 1;
    for (int i = 0; i < n; ++i) {
        M *= m[i];
    }

    ll result = 0;
    for (int i = 0; i < n; ++i) {
        ll Mi = M / m[i];
        ll ti = mod_inverse(Mi, m[i]);
        result = (result + a[i] * Mi * ti) % M;
    }

    return (result + M) % M;
}

int main() {
    // 示例：《孙子算经》问题
    // x === 2 (mod 3)
    // x === 3 (mod 5)
    // x === 2 (mod 7)
    std::vector<ll> a = {2, 3, 2};
    std::vector<ll> m = {3, 5, 7};

    ll solution = chinese_remainder_theorem(a, m);

    std::cout << "方程组的解是: " << solution << std::endl; // 应该输出 23

    return 0;
}
```

## 测试用例

我们使用《孙子算经》中的经典问题作为测试用例。
**问题**:
$$
\begin{cases}
x \equiv 2 \pmod 3 \\
x \equiv 3 \pmod 5 \\
x \equiv 2 \pmod 7
\end{cases}
$$

**手动演算**:
1.  **模数**: $m_1=3, m_2=5, m_3=7$。
2.  **总模**: $M = 3 \times 5 \times 7 = 105$。
3.  **计算各项**:
    - **i = 1**:
      - $a_1=2, m_1=3$
      - $M_1 = M/m_1 = 105/3 = 35$
      - 求 $35 \cdot t_1 \equiv 1 \pmod 3$。因为 $35 \equiv 2 \pmod 3$，即求 $2 \cdot t_1 \equiv 1 \pmod 3$。解得 $t_1=2$。
      - 本项贡献: $a_1 M_1 t_1 = 2 \times 35 \times 2 = 140$。
    - **i = 2**:
      - $a_2=3, m_2=5$
      - $M_2 = M/m_2 = 105/5 = 21$
      - 求 $21 \cdot t_2 \equiv 1 \pmod 5$。因为 $21 \equiv 1 \pmod 5$，即求 $1 \cdot t_2 \equiv 1 \pmod 5$。解得 $t_2=1$。
      - 本项贡献: $a_2 M_2 t_2 = 3 \times 21 \times 1 = 63$。
    - **i = 3**:
      - $a_3=2, m_3=7$
      - $M_3 = M/m_3 = 105/7 = 15$
      - 求 $15 \cdot t_3 \equiv 1 \pmod 7$。因为 $15 \equiv 1 \pmod 7$，即求 $1 \cdot t_3 \equiv 1 \pmod 7$。解得 $t_3=1$。
      - 本项贡献: $a_3 M_3 t_3 = 2 \times 15 \times 1 = 30$。
4.  **合并解**:
    - $x = 140 + 63 + 30 = 233$。
5.  **求最小正整数解**:
    - $x \pmod{105} = 233 \pmod{105} = 23$。

**代码运行结果**:
```
方程组的解是: 23
```
结果与手动演算一致。

## 经典例题

#### 1. 洛谷 P1495 【模板】中国剩余定理(CRT)/曹冲养猪
*   **链接**: [Luogu P1495](https://www.luogu.com.cn/problem/P1495)
*   **描述**: 这是一个最直接的 CRT 模板题。题目给出一系列 $x \equiv a_i \pmod{m_i}$ 的方程，其中 $m_i$ 两两互质，要求求出最小的正整数解 $x$。
*   **解题思路**: 直接套用上面给出的 CRT 代码模板即可解决。注意数据范围可能需要使用 `long long`。

#### 2. 洛谷 P3868 [TJOI2009]猜数字
*   **链接**: [Luogu P3868](https://www.luogu.com.cn/problem/P3868)
*   **描述**: 题目给出一组数 $a_i$，要求找到一个最小的 $x$ 满足对于所有的 $i$，都有 $x \equiv a_i \pmod{b_i}$，其中 $b_i$ 是一组给定的数。这道题的模数 $b_i$ 不一定两两互质。
*   **解题思路**: 这是对 CRT 的扩展，需要使用 **扩展中国剩余定理 (EXCRT)**。EXCRT 的思想是逐个合并方程。假设我们已经求出了前 $k-1$ 个方程的解 $x_{k-1}$，通解为 $x = x_{k-1} + t \cdot M_{k-1}$ (其中 $M_{k-1}$ 是前 $k-1$ 个模数的最小公倍数)。现在要满足第 $k$ 个方程，即 $x_{k-1} + t \cdot M_{k-1} \equiv a_k \pmod{m_k}$。这是一个关于 $t$ 的线性同余方程，可以用扩展欧几里得算法求解。

#### 3. POJ 1006 Biorhythms
*   **链接**: [POJ 1006](http://poj.org/problem?id=1006)
*   **描述**: 人有体力、情感、智力三个周期，分别为 23, 28, 33 天。给定三个周期的高峰出现的日子 $p, e, i$ 和一个起始日期 $d$，要求计算从第 $d+1$ 天开始，下一次三个周期同时达到高峰是哪一天。
*   **解题思路**: 设下一次高峰在第 $x$ 天，则问题可以转化为求解同余方程组：
    $$
    \begin{cases}
    x \equiv p \pmod{23} \\
    x \equiv e \pmod{28} \\
    x \equiv i \pmod{33}
    \end{cases}
    $$
    由于 23, 28, 33 两两互质，这是一个标准的 CRT 问题。解出最小的正整数解 $x_0$ 后，如果 $x_0 \le d$，则需要加上周期的最小公倍数，直到 $x > d$。

## 实践思考与扩展

!!! success "扩展：模数不互质的情况"
当模数 $m_1, m_2, \dots, m_n$ 不满足两两互质时，标准的中国剩余定理不再适用。此时需要使用 **扩展中国剩余定理 (Extended Chinese Remainder Theorem, EXCRT)**。

EXCRT 的核心思想是**两两合并**。
1.  假设我们要求解两个方程：
    $$
    \begin{cases}
    x \equiv a_1 \pmod{m_1} \\
    x \equiv a_2 \pmod{m_2}
    \end{cases}
    $$
2.  由第一个方程，可设 $x = a_1 + k \cdot m_1$。
3.  代入第二个方程，得到 $a_1 + k \cdot m_1 \equiv a_2 \pmod{m_2}$。
4.  整理得 $k \cdot m_1 \equiv a_2 - a_1 \pmod{m_2}$。
5.  这是一个关于 $k$ 的线性同余方程，可以使用扩展欧几里得算法求解。
    - 如果方程无解 (当 $\gcd(m_1, m_2)$ 不能整除 $a_2 - a_1$ 时)，则原方程组无解。
    - 否则，解出 $k$ 的一个特解 $k_0$，则 $k$ 的通解为 $k = k_0 + t \cdot \frac{m_2}{\gcd(m_1, m_2)}$。
6.  将 $k$ 的通解代回 $x = a_1 + k \cdot m_1$，得到 $x$ 的新表达式，这个表达式实际上等价于一个新的同余方程 $x \equiv a' \pmod{M'}$，其中 $M' = \text{lcm}(m_1, m_2)$。
7.  重复此过程，将这个新方程与第三个方程合并，直到所有方程合并完毕。

这种方法更加通用，但实现起来也相对复杂。

