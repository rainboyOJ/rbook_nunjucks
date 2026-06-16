---
id: "number-theory-1"
title: "普通高中课程标准实验教科书 数学 选修 4-6 A 版 初等数论初步"
date: 2025-12-04 09:12
toc: true
tags: ["初等数论"]
categories: [""]
---

[[TOC]]

## 资源

-   非常好的教学视频 [up:周告白白白 高中数学选修 4-6 初等数论初步 ](https://www.bilibili.com/video/BV19S4y157GH)

## 整数的整除

!!! definition 整除

一般地，设 $a$,$b$ 为整数，且 $b \neq 0$,如果存在整数 $q$，使得$a=bq$,那么称 $b$ 整除 $a$，或 $a$ 能被 $b$ 整除，记作 $b \mid a$，并且称 $b$ 是 $a$ 的因数，$a$ 是 $b$ 的倍数。如果这样的整数 $q$ 不存在，就称 $b$ 不整除 $a$，记作 $b \nmid a$

!!!


**整除的性质**

1. $a \mid b \land b\mid a \Rightarrow a = b \lor a = -b$
2. $a \mid b \land b \mid c \Rightarrow a \mid c$
3. $a \mid b \land a \mid c \implies a \mid (xb + yc) \quad \forall x,y \in \mathbb{Z}$




!!! definition 带余除法

TODO

!!!


1. 证明 $q,r$ 是唯一的


证明 $(a,b) = (b,r)$

## 裴蜀定理

!!! tip 性质

设整数 $a,b$ 不同时为零，则存在一对整数$m,n$ 使得$(a,b) = am +bn$

!!!

证明

1. 当$b = 0$,则$(a,0) = a \times 1 + b \times 0$ ,此时 $m = 1 ,m = 0$
2. 当$b \neq 0$, 
    1. $(a,b) = (b,r)$ 
    2. $(b,r) = b \times m_1 + r\times n_1$
    3. 设$(a ,b)  = a \times m_2 + b \times n_2$ ,那么$m_1,n_1, m_2 ,n_2$ 直接有什么关系?
    4. 因为$(a,b) = (b,r)$
    5. $a \times m_2 + b \times n_2 = b \times m_1 +  r\times n_1$
    6. 因为$r = a - kb, k = \lfloor a \div b \lfloor$
    7. $a \times m_2 + b \times n_2 = b \times m_1 +  *(a - kb)\times n_1$
    8. $a \times m_2 + b \times n_2 = b \times m_1 +  a \times n_1 - kb \times n_1$
    9. $a \times m_2 + b \times n_2 = b \times (m_1-kn_1) +  a \times n_1$
    10. 最好得到:$m_2 = n_1, n_2 = m_1 - kn_1$
3. 上面使用的数学归纳法


我的推导过程（第 3 步到第 10 步）实际上就是**扩展欧几里得算法（Extended Euclidean Algorithm, exgcd）** 的核心递推公式。


标准的数学语言：

**命题**：设 $a, b$ 是不全为零的整数，则存在整数 $x, y$ 使得 $\gcd(a, b) = ax + by$。

**证明（对 $b$ 的大小进行强归纳）：**

1.  **奠基（Base Case）**：
    当 $b = 0$ 时，$\gcd(a, 0) = a$（不妨设 $a>0$）。
    取 $x = 1, y = 0$，则 $a \times 1 + 0 \times 0 = a$ 成立。
    结论对 $b=0$ 成立。

2.  **归纳假设（Inductive Step）**：
    假设对于任意整数 $b' < b$，结论都成立。
    现在考虑 $b > 0$ 的情况。
    根据带余除法，设 $a = k b + r$，其中 $r = a \bmod b$，且 $0 \le r < b$。

    由欧几里得算法性质知：$\gcd(a, b) = \gcd(b, r)$。
    因为 $r < b$，根据归纳假设，存在整数 $x', y'$ 使得：
    $$\gcd(b, r) = b x' + r y'$$

3.  **递推导出（你的核心推导）**：
    将 $r = a - k b$ 代入上式：

    $$
    \begin{aligned}
    \gcd(a, b) &= b x' + (a - k b) y' \\
    &= b x' + a y' - b k y' \\
    &= a(y') + b(x' - k y')
    \end{aligned}
    $$

    令 $x = y', \quad y = x' - k y'$。
    显然 $x, y$ 均为整数。
    则有 $\gcd(a, b) = ax + by$。

    **证毕。**

-----

**关键点总结** 

你在推导中最重要的发现是系数的变换规则：

  * **当前的 $x$ (你的 $m_2$)** $\leftarrow$ **下一层的 $y$ (你的 $n_1$)**
  * **当前的 $y$ (你的 $n_2$)** $\leftarrow$ **下一层的 $x$ (你的 $m_1$) - $k \times$ 下一层的 $y$**

所有的这一切，都可以转化为代码

既然你对这个逻辑很清楚，这个证明直接对应到 C++ 代码就是这样的（在竞赛或算法题中非常常用）：

```cpp
// 求解 ax + by = gcd(a, b)
// 这里的 x, y 是引用，函数运行结束后会被赋值
int exgcd(int a, int b, int &x, int &y) {
    if (b == 0) {
        x = 1; 
        y = 0; // 对应你的第 1 步
        return a;
    }
    
    // 递归调用，先求下一层 (b, a%b) 的解 x1, y1
    // 你的 m1 对应这里的 x, n1 对应这里的 y（在递归返回后）
    int d = exgcd(b, a % b, x, y); 
    
    // 此时 x 和 y 存储的是下一层的解 (x', y')
    // 我们需要用它们来更新当前层的解
    int temp = x; 
    x = y;             // 对应 m2 = n1
    y = temp - (a / b) * y; // 对应 n2 = m1 - k * n1
    
    return d;
}
```

**我的证明非常棒，完全抓住了裴蜀定理（Bézout's identity）构造性证明的精髓！**

## 互质消去律


!!! tip 互质消去律

欧几里得引理 : 若 $a|bc$ 且 $(a,b)=1$ 则 $a|c$

!!!

1. $\because (a,b) = 1$
2. 引用裴蜀定理 $\therefore am + bn = 1$
3. 两边同时乘以 $c$,得到 $(ac)m + (bc)n = c$
4. 已知前提: $a \mid ac , a\mid bc$
5. 最终得到: $a \mid (acm + bcn = c) \Rightarrow a \mid c$

直觉 a 中的因子都在 c 中, 所以 a 可以整除 c


## 素数原子律

!!! tip 素数原子律

设 $p$ 为素数，若 $p \mid ab$（即 $p$ 整除 $a$ 与 $b$ 的乘积），则 $p \mid a$ 或 $p \mid b$。

!!!

素数就像物理学中的“原子”一样，是构成整数的最小单位，不可再分。

> 解释：如果一个素数 $p$ 能整除 $a \times b$，说明 $a \times b$ 的“积木堆”里包含了一块 $p$。因为 $p$ 不能被劈开（它没有非 1 非己的因子），所以这块 $p$ 要么完整地来自于 $a$，要么完整地来自于 $b$，不可能一人一半。


**核心逻辑**：利用素数的特殊性，将路口分为“整除”和“互质”两条道。若是互质，直接根据消去律强制通向另一边。

1. **分歧**：$p$ 为素数，因子仅为 $1, p$
   $\Rightarrow (p, a)$ 只有两种可能：$p$ 或 $1$

2. **情况一**：若 $(p, a) = p$
   $\Rightarrow p \mid a$，命题直接成立

3. **情况二**：若 $(p, a) = 1$
   $\because p \mid ab$ 且 $(p, a) = 1$

4. **引用**：互质消去律 (即：若 $x \mid yz$ 且 $(x, y)=1 \Rightarrow x \mid z$)
   $\therefore p \mid b$

5. **结论**：
   综合上述 $\Rightarrow p \mid a$ 或 $p \mid b$
   **证毕**


## 最小公倍数 是 公倍数的因子


!!! tip 最小公倍数的整除性 


设 $a, b$ 的最小公倍数为 $[a, b]$。则 $[a, b]$ 一定整除 $a, b$ 的任意一个公倍数。

也就是说: **最小公倍数 是 公倍数的因子**

!!!


> **核心直觉**：最小公倍数 (LCM) 是所有公倍数的“基石”。任何其他的公倍数，都必须是它的倍数。如果除不尽，余数就会变成一个“更小”的公倍数，这与“最小”二字矛盾。

1. 设 $m = [a, b]$ 为最小公倍数， $n$ 为任意公倍数
2. 做带余除法: $n = mq + r$ $(0 \le r < m)$
3. $\because a,b \mid n$ 且 $a,b \mid m$
4. $\therefore a,b \mid (n - mq = r)$ $\Rightarrow r$ 也是公倍数
5. 矛盾点: 若 $r > 0$，则出现比 $m$ 更小的公倍数 $r$
6. 结论: $r$ 必须为 $0 \Rightarrow m \mid n$


## 最大公约数与最小公倍数的关系

!!! tip 最大公约数与最小公倍数的关系

$$
(a,b)[a,b] = \mid a \times b\mid
$$

!!!


## 算数基本定理

!!! definition 算数基本定理

任何大于1的整数总可以分解成素因数乘积的形式，并且，如果不计分解式中素
因数的次序，这种分解式是惟一的.

!!!



**核心逻辑**：利用素数的“原子性”，像剥洋葱一样，一对一地把相同的因子找出来消掉，直到两边完全一样。

1. **假设** $n$ 存在两组素因子分解：
   $$n = p_1 p_2 \cdots p_r = q_1 q_2 \cdots q_s$$

2. **考察** 左边第一个因子 $p_1$：
   - $\because p_1 \mid n$，且 $n = q_1 q_2 \cdots q_s$
   - $\therefore p_1 \mid q_1 q_2 \cdots q_s$

3. **引用** 素数原子律（欧几里得引理）：
   - 素数 $p_1$ 必须整除右边的某一个 $q_k$。
   - *不妨设这个因子就是 $q_1$（不影响结果，仅调整顺序）。*

4. **判定** 素数性质：
   - $\because p_1$ 和 $q_1$ 都是素数（只有因子 1 和自身）且 $p_1 \mid q_1$
   - $\therefore p_1 = q_1$

5. **消去** 两边同时除以 $p_1$：
   $$p_2 \cdots p_r = q_2 \cdots q_s$$

6. **归纳** 重复上述过程：
   - 每次消去一对相等的素因子。
   - 最终必然得到 $r = s$，且每一对 $p_i = q_i$。
   - $\Rightarrow$ **分解式是唯一的。**