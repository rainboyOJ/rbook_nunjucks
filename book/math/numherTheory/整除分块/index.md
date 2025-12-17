---
id: "div-block"
title: "整除分块"
date: 2025-11-24 15:30
toc: true
tags: ["算法", "数论", "整除分块"]
categories: [""]
---

[[TOC]]

## 摘要 (Abstract)

整除分块（也称整除区间分块或 Harmonic Lemma）是竞赛数论中一个常用技巧，用于在 $O(\sqrt{n})$ 时间内计算涉及 $\lfloor n/i\rfloor$ 的和式。本文从直观出发，给出公式推导、伪代码与可运行 C++ 模板，并展示若干应用与训练题目。

## 背景与动机 (Motivation)

在许多数论与计数问题中，会遇到类似

$$
\mathrm{Ans}=\sum_{i=1}^{n} \left\lfloor\frac{n}{i}\right\rfloor
$$

或

$$
\sum_{i=1}^{\min(n,m)}\left\lfloor\frac{n}{i}\right\rfloor\left\lfloor\frac{m}{i}\right\rfloor
$$

的求和。直接按 $i$ 逐项计算为 $O(n)$，当 $n$ 很大（如 $10^{12}$）时不可行。整除分块利用 $\lfloor n/i\rfloor$ 在区间上保持常数的性质，把 $1..n$ 分成 $O(\sqrt{n})$ 个块，从而把复杂度降低到 $O(\sqrt{n})$，是许多高阶算法（如杜教筛、求 GCD 和 源码题）的基础。

## 问题定义 (Problem Definition)

给定整数 $n$（或两个整数 $n,m$），高效计算涉及 $\lfloor n/i\rfloor$ 的和式，例如

- 单变量： $S(n)=\sum_{i=1}^{n} \left\lfloor\dfrac{n}{i}\right\rfloor$。
- 双变量： $T(n,m)=\sum_{i=1}^{\min(n,m)} \left\lfloor\dfrac{n}{i}\right\rfloor\left\lfloor\dfrac{m}{i}\right\rfloor$。

目标：在 $O(\sqrt{n})$ 或 $O(\sqrt{\min(n,m)})$ 时间内计算答案。

## 一句话算法

将 $1..n$ 分成若干段 $[l,r]$，使得在每段内 $\left\lfloor\dfrac{n}{i}\right\rfloor$ 恒为常数 $k$，直接按段累加 $k\times (r-l+1)$。

## 关键思路 (Key Idea)

令当前段左端点为 $l$，取 $k=\left\lfloor\dfrac{n}{l}\right\rfloor$。在满足 $\left\lfloor\dfrac{n}{i}\right\rfloor=k$ 的最大 $i$ 处为

$$
r=\left\lfloor\dfrac{n}{k}\right\rfloor=\left\lfloor\dfrac{n}{\left\lfloor n/l\right\rfloor}\right\rfloor.
$$

因此可以从 $l$ 跳到 $r+1$，将区间 $[l,r]$ 的贡献一次性计算出来。

## 算法步骤 (Algorithm Steps)

伪代码（单变量）：

```cpp
for (long long l = 1, r; l <= n; l = r + 1) {
    long long k = n / l;            // 当前块的商
    r = n / k;                      // 当前块的右端点，满足 floor(n/i) == k
    ans += k * (r - l + 1);         // 累加整段贡献
}
```

在实现中要注意使用整型（long long），并保证循环终止条件正确。

## 算法证明 (Proof)

设 $k=\lfloor n/l\rfloor$，则 $k\le n/l < k+1$，两边取倒数并乘以 $n$ 得：

$$
\frac{n}{k+1} < l \le \frac{n}{k}.
$$

因此所有满足 $\lfloor n/i\rfloor=k$ 的整数 $i$ 都在区间 $\left(\dfrac{n}{k+1},\,\dfrac{n}{k}\right]$ 内，最大整数为 $\lfloor n/k\rfloor$，即上面所给的 $r$。由此可将 $1..n$ 分成若干不重叠的区间，覆盖全部 $i$，每个区间计算一次即可。

## 复杂度分析 (Complexity Analysis)

时间复杂度：每次循环要么使 $l$ 增加（当 $l\le\sqrt{n}$）最多 $\sqrt{n}$ 次；要么 $k=n/l<\sqrt{n}$，不同的 $k$ 值也最多有 $\sqrt{n}$ 个。因此总循环次数为 $O(\sqrt{n})$，每次常数时间，整体为 $O(\sqrt{n})$。

空间复杂度：$O(1)$，只使用常数额外空间。

## 代码实现 (C++)

下面给出竞赛常用的、风格简洁、注释清晰的 C++ 实现：

```cpp
#include <bits/stdc++.h>
using namespace std;
using int64 = long long;

// 计算 S(n) = sum_{i=1}^n floor(n / i)
int64 sum_div_blocks(int64 n) {
    int64 ans = 0;
    for (int64 l = 1, r; l <= n; l = r + 1) {
        int64 k = n / l;            // 当前块的商
        r = n / k;                  // 当前块的右端点，满足 floor(n/i) == k
        // r 不会小于 l，且 r >= l
        ans += k * (r - l + 1);
    }
    return ans;
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    long long n;
    // 输入一个 n，输出 S(n)
    while (cin >> n) {
        cout << sum_div_blocks(n) << '\n';
    }
    return 0;
}
```

注：在多变量分块（例如同时考虑 n 和 m）时，右端点应取两者决定右端点的最小值：

```cpp
int64 limit = min(n, m);
for (int64 l = 1, r; l <= limit; l = r + 1) {
    int64 a = n / l;
    int64 b = m / l;
    r = min(limit, min(n / a, m / b));
    ans += a * b * (r - l + 1);
}
```

## 测试用例 (Test Case)

示例：

输入：
```
10
```

运行主程序输出：
```
27
```

手动验证：对于 n=10，各项为 10,5,3,2,2,1,1,1,1,1，和为 27。

边界测试：
- n=1 -> 输出 1
- n=10^12（在 64 位整型下可行） -> 运行时间约为 O(10^6) 级别操作数，通常能通过竞赛限制。

## 实践思考与扩展 (Further Thinking & Extension)

- 多变量分块：用于处理类似 $\sum_{i=1}^{\min(n,m)}\lfloor n/i\rfloor\lfloor m/i\rfloor$ 的问题，按共同不变区间跳跃。
- 与杜教筛结合：当计算与素数、莫比乌斯函数等相关的前缀和时，整除分块可以把复杂度从 $O(n)$ 降到 $O(\sqrt{n})$，配合筛法能处理 $n$ 高达 $10^{12}$ 的问题。
- 注意整数溢出：在计算乘法或累加时，一定要使用 64 位（long long / int64）。

开放问题：如何在更复杂的多重求和中（例如三重求和）高效利用分块思想？如何在并行或向量化环境下实现整除分块以进一步加速？

## 经典例题 (Classic Problems)

1.  [[problem: luogu,P2261]] ：利用 $k\bmod i = k - i\lfloor k/i\rfloor$ 转化为求 $\sum \lfloor k/i\rfloor$。
   - 练习要点：把余数和转换后用单变量分块求解。
2. UVa 11526 - H(n)：直接求 $\sum_{i=1}^{n} \lfloor n/i\rfloor$，用于验证模板正确性。
3. 洛谷 P2260 清华集训2012 模积和：涉及多变量分块与取模处理。

每道题的解题思路都以“构造分块区间 $[l,r]$，计算区间贡献”为核心，遇到多变量时取共同右端点的最小值。


!!! note "小结"
整除分块是竞赛编程中非常实用的技巧，掌握后能显著提升处理含有 $\lfloor n/i\rfloor$ 型求和问题的能力。推荐通过 UVa 11526 与洛谷 P2261 等题目反复练习。
!!!

