--- 
id: "inverse"
title: "模逆元"
date: 2025-12-05 10:22
toc: true
tags: ["TODO"]
categories: [""]
code_template:
  - title: exgcd求逆元
    desc: "递归反着传 (y, x)，回来减乘除 (y -= a/b*x)"
    tags: ["数论","逆元"]
    code: /code/math/inv_exgcd.cpp
  - title: 线性求逆元
    desc: "一个数字的逆元等于它的商的负数乘以它的余数的逆元"
    tags: ["数论","逆元"]
    code: /code/math/line_inv.cpp
---

[[TOC]]

## 概念

模逆元是一个数在模意义下的逆元，即对于一个整数 $a$ 和模数 $m$，如果存在一个整数 $x$，使得 $a \times x \equiv 1 \pmod{m}$，那么 $x$ 就是 $a$ 在模 $m$ 下的逆元，记作 $a^{-1}$。

逆元存在的 **充要条件** 是 $a$ 和 $m$ 互质，即 $\gcd(a, m) = 1$。

证明过程见 [一次同余方程有解的充要条件](../../4-6初等数论初步/chapter2.md)

## 概念2

证明: 除以一个数取模等于乘以这个数的逆元取模

$$
\frac{a}{b} \equiv a \times b^{-1} \pmod{m}
$$

这是为您准备的证明，采用**极简算式推导流**。

**前提条件**：$b$ 与模数 $m$ 互质（即 $\gcd(b, m) = 1$），这样 $b$ 的逆元 $b^{-1}$ 才存在。

证明：模除法转化为乘逆元

1.  **定义** (将除法转化为乘法方程)：
    - 设 $x$ 为 $a$ 除以 $b$ 的结果：
    - $x \equiv \frac{a}{b} \pmod m$
    - 使用**同余相乘的运算性质**,两边同时乘以 $b$：
    - $b \cdot x \equiv a \pmod m$
2.  **操作** (两边同乘逆元)：
    - 在等式两边同时乘以 $b$ 的逆元 $b^{-1}$：
    - $b^{-1} \cdot (b \cdot x) \equiv b^{-1} \cdot a \pmod m$

3.  **结合** (利用乘法结合律)：
    - $(b^{-1} \cdot b) \cdot x \equiv a \cdot b^{-1} \pmod m$

4.  **消去** (利用逆元定义)：
    - 因为 $b^{-1} \cdot b \equiv 1 \pmod m$：
      - $x \equiv x \pmod m$
      - $$
        \begin{align*}
        (b^{-1} \cdot b) \cdot x &\equiv 1 \cdot x \pmod m \\
        &\equiv x \pmod m
        \end{align*}
        $$
    - $1 \cdot x \equiv a \cdot b^{-1} \pmod m$

5.  **结论**：
    - $x \equiv a \times b^{-1} \pmod m$

**证毕**

1. 证明反复使用了 同余的运算性质: 同余相乘.
2. 直觉: 除以一个数等于乘以这个数的逆元(倒数).

## exgcd 求逆元

求逆元,就是求解方程 $a \cdot x \equiv 1 \pmod{m}$ 的解 $x$。

$$
  \begin{align*}
  &a \cdot x \equiv 1 \pmod{m} \\
  &a \cdot x - m \cdot y = 1
  \end{align*}
$$

@include-code(/code/math/inv_exgcd.cpp, cpp)


## 题目

- [[problem: 51Node,1256]]

##  线性求逆元

学习“线性求逆元”算法，重点在于理解它的**应用场景**和那个**神奇的递推公式**。

为了让你**心智负担最低**，我把它拆解为三个步骤：**为什么用**、**公式怎么来的**、**代码怎么写**。

-----


**如果题目要你求 $1, 2, 3, \dots, n$ 所有数的逆元呢？**

  * 用 `exgcd` 算 $n$ 次：总耗时 $O(n \log p)$。如果 $n$ 是 $10^7$ 级别，会超时。
  * 用 **线性算法**：总耗时 $O(n)$。**平均求一个逆元只要 $O(1)$ 的时间！**

> **结论**：当你需要**批量**计算一堆逆元（比如求组合数 $C(n, m) \% p$ 需要用到很多阶乘的逆元）时，必须用线性算法。

-----

极简推导流 (公式是怎么来的)

设模数为 $p$，我们需要求 $i$ 的逆元 $i^{-1}$。

1.  **带余除法**：
    - 令 $p = k \cdot i + r$
    - 其中 $k = \lfloor \frac{p}{i} \rfloor$ (商)，$r = p \bmod i$ (余数)。

2.  **列同余式**：
    - $k \cdot i + r \equiv 0 \pmod p$
    - *(因为 $k \cdot i + r$ 等于 $p$，而 $p$ 模 $p$ 是 0)*

3.  **神奇操作** (两边同时乘 $i^{-1} \cdot r^{-1}$ )：
    - $k \cdot r^{-1} + i^{-1} \equiv 0 \pmod p$

4.  **移项求 $i^{-1}$**：
    - $i^{-1} \equiv -k \cdot r^{-1} \pmod p$
    - $i^{-1} \equiv -\lfloor \frac{p}{i} \rfloor \times (p \bmod i)^{-1} \pmod p$

5.  **代回变量**：

      * $k$ 替换为 $\lfloor \frac{p}{i} \rfloor$
      * $r$ 替换为 $p \bmod i$
      * $-k$ 在模运算中写成 $p - k$ 以保证正数
6. **边界** 
  - $1^{-1} \equiv 1 \pmod p$
  - 当 $i=1$ 时，逆元就是 $1$。

**最终公式**：

$$inv[i] = (p - \lfloor \frac{p}{i} \rfloor) \times inv[p \bmod i] \% p$$


这个公式说明: 一个数字的逆元等于它的商的负数乘以它的余数的逆元。

$$ i^{-1} \equiv -\lfloor \frac{p}{i} \rfloor \times (p \bmod i)^{-1} \pmod p$$

- 一个数字的逆元：$i^{-1}$
- 等于：$\equiv$
- 它的商的负数：$-\lfloor \frac{p}{i} \rfloor$
- 乘以：$\times$
- 它的余数的逆元：$(p \bmod i)^{-1}$

-----

时间复杂度: $O(log2P)$ ,证明略.

### 3\. 代码模版 (背诵版)

这个算法是**递推**的。

  * 因为 $p \bmod i$ 肯定比 $i$ 小，所以算 $inv[i]$ 时，$inv[p \bmod i]$ 肯定已经算出来了。
  * **初始条件**：$1$ 的逆元永远是 $1$，即 `inv[1] = 1`。


```cpp
// 线性求 1 到 n 所有数的逆元
// 时间复杂度 O(n)
#include <iostream>
#include <vector>
using namespace std;

long long inv[2000005]; // 数组开到最大范围
long long p; // 模数

void init_inverse(int n) {
    inv[1] = 1; // 【奠基】1的逆元是1
    
    for (int i = 2; i <= n; ++i) {
        // 【核心公式】
        // inv[i] = (p - p/i) * inv[p % i] % p
        inv[i] = (p - p / i) * inv[p % i] % p;
    }
}

int main() {
    int n;
    cin >> n >> p;
    init_inverse(n);
    
    // 输出验证
    for(int i=1; i<=n; i++) {
        cout << inv[i] << "\n";
    }
    return 0;
}
```

-----

### 🧠 记忆钩子

怎么记住 `(p - p / i) * inv[p % i]` 这一长串？

1.  **结构**：它是 **"商"** $\times$ **"余数的逆元"**。
2.  **符号**：因为移项时有个负号，所以用 `p - ...` 来处理负数。
3.  **依赖**：我要算 $i$ 的逆元，我得问问比我小的 $p \% i$ 的逆元是多少。

### ✅ 学习路线建议

1.  **抄写一遍推导**：不用死记硬背，自己在纸上从 $k \cdot i + r \equiv 0$ 推导一遍，你就永远忘不了了。
2.  **AC 一道板子题**：
      * **洛谷 P3811 【模板】乘法逆元**
      * 这就强迫你必须写出 $O(n)$ 的线性算法，用 `exgcd` 会超时。
3.  **整合进组合数**：
      * 以后求 $C(n, m) \pmod p$，先预处理阶乘，再线性求阶乘的逆元。



## 参考

- [oi-wiki 模逆元](https://oi-wiki.org/math/number-theory/inverse/)