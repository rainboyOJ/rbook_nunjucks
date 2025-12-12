---
id: "manacher"
title: "Manacher：线性时间最长回文子串"
date: 2025-12-09 14:02
toc: true
tags: ["字符串", "回文", "算法"]
categories: ["字符串"]
code_template:
  - title: 马拉车
    desc: "最长回文子串"
    tags: ["manacher"]
    code: /code/string/manacher.cpp
---

[[TOC]]

## 摘要

Manacher 算法在 $O(n)$ 时间内找到字符串的最长回文子串。它通过在字符串间插入分隔符并维护当前回文区间的右端点 $R$ 与中心 $c$，利用对称性快速初始化半径，从而避免重复比较。

## 背景与动机

在许多字符串问题中，我们需要判断或寻找回文子串。朴素方法对每个中心向两侧扩展需要 $O(n^2)$ 时间，Manacher 算法将复杂度降为线性，使其在竞赛和工程中都非常实用，例如文本处理、模式匹配、最长回文检测等场景。

## 问题定义

给定一个长度为 $n$ 的字符串 $s$，求其中最长的回文子串，返回其长度和在原串中的起止位置（0-based，左闭右闭）。

## 关键思路

- 在原字符串字符间插入统一的分隔符（例如 `#`），并在两端加哨兵，所有回文长度统一为奇数，避免奇偶情况分裂。
- 维护当前已知的最右回文边界 $R$ 和其中心 $c$。对于新中心 $i$，利用其镜像位置 $mirror=2c-i$ 的结果来初始化半径，从而跳过已知匹配。
- 只有在超出已知区间时才做暴力扩展。整体比较次数被严格限制，从而达到线性时间。

## 一句话算法

将字符串变换为带分隔符的形式，维护当前最右回文区间，利用镜像初始化半径并仅在必要时扩展。

回文串的镜像也是回文的

## 算法步骤

1. 预处理：构造新串 `t = ^ # s[0] # s[1] # ... # $`。
2. 初始化：数组 `p` 存储每个位置的半径，初始均为 0；变量 `center = 0, right = 0`。
3. 遍历 `i` 从 `1` 到 `m-2`：
   - 计算镜像 `mirror = 2*center - i`。
   - 若 `i < right`，则 `p[i] = min(right - i, p[mirror])`，否则 `p[i] = 0`。
   - 在 `t` 上以 `i` 为中心暴力扩展：当 `t[i+1+p[i]] == t[i-1-p[i]]` 时 `p[i]++`。
   - 若 `i + p[i] > right`，更新 `center = i, right = i + p[i]`。
4. 从 `p` 中找到最大值 `best_len` 和对应中心 `best_center`，映射回原串得到起止索引：
   - 起点 $l = (best\_center - best\_len) / 2$，终点 $r = l + best\_len - 1$。

### 伪代码

```
build(s):
  t = ^ # s[0] # s[1] # ... # $
  p[0..|t|-1] = 0
  center = right = 0
  for i = 1 .. |t|-2:
    mirror = 2*center - i
    if i < right: p[i] = min(right - i, p[mirror])
    else p[i] = 0
    while t[i+1+p[i]] == t[i-1-p[i]]: p[i]++
    if i + p[i] > right: center = i; right = i + p[i]
  end
  answer from p
```

## 算法证明

### 正确性（直觉）

- 变换串 `t` 将原串的奇偶回文统一为奇回文，任何原串回文在 `t` 上对应一个以分隔符为间隔的回文段。
- 对于位置 `i`，若已知中心 `c` 的回文区间覆盖 `i`（即 $i< R$），则 `i` 关于 `c` 的镜像位置 `mirror=2c-i` 的回文信息可以被利用：因为区间对称，`p[mirror]` 提供了 `p[i]` 的下界，剩余部分只需扩展验证。

### 时间复杂度证明（极简算式推导流 + 直觉模型）

直觉模型：把右边界 $R$ 想象成一台“推土机”，只向右推进，不回头。每次扩展成功时推土机向右移动；每次扩展失败，当前中心停止扩展。

算式推导流：将字符比较分为两类。

1. 扩展成功（使 $R$ 向右移动）：$R$ 最多从最左推进到最右，移动次数上界为 $|t| = 2n+3$（含哨兵），记为 $O(n)$。
2. 扩展失败（每个中心终止时发生一次）：每个中心最多发生一次失败，中心数量为 $O(n)$。

因此，总比较次数为 $O(n) + O(n) = O(n)$，即线性时间。

## 复杂度分析

- 时间复杂度：$O(n)$，其中 $n$ 为原串长度。
- 空间复杂度：$O(n)$，用于存储变换串和半径数组。

## 代码实现

下面给出不使用 `std::string` 的竞赛风格实现（C 风格字符串，固定缓冲）。代码放在 `code/string/manacher.cpp`，并在这里给出完整实现：

```cpp
#include <bits/stdc++.h>
using namespace std;

constexpr int MAXN = 110000;            // 原始字符串最大长度

// Manacher 模板（不使用 std::string）
struct Manacher {
    static const int SZ = MAXN * 2 + 5; // 变换后最大长度
    char t[SZ];    // 变换后的字符串：^ # a # b # ... # $
    int p[SZ];     // 半径数组（在 t 上的扩展长度）
    int m = 0;     // t 的实际长度（含终止符）

    // 构造变换串并计算 p[]
    // 输入：C 风格字符串 s（以 '\0' 结尾）
    void build(const char *s) {
        int n = (int)strlen(s);
        int k = 0;
        t[k++] = '^';   // 左哨兵，防止越界
        t[k++] = '#';
        for (int i = 0; i < n; ++i) {
            t[k++] = s[i];
            t[k++] = '#';
        }
        t[k++] = '$';   // 右哨兵
        t[k] = '\0';
        m = k;

        // 初始化半径数组
        for (int i = 0; i < m; ++i) p[i] = 0;

        // Manacher 主循环
        int center = 0, right = 0;
        for (int i = 1; i < m - 1; ++i) {
            int mirror = 2 * center - i;
            if (i < right) p[i] = min(right - i, p[mirror]);
            else p[i] = 0;

            // 暴力扩展：安全因为有哨兵
            while (t[i + 1 + p[i]] == t[i - 1 - p[i]]) ++p[i];

            // 更新中心与右边界
            if (i + p[i] > right) {
                center = i;
                right = i + p[i];
            }
        }
    }

    // 获取最长回文子串，返回长度；通过引用参数 l,r 返回原串上的左闭右闭索引（0-based）
    int longest(int &l, int &r) const {
        int best_len = 0, best_center = 0;
        for (int i = 1; i < m - 1; ++i) {
            if (p[i] > best_len) {
                best_len = p[i];
                best_center = i;
            }
        }
        if (best_len == 0) { l = 0; r = -1; return 0; }
        // 将 t 中的位置映射回原串的索引
        l = (best_center - best_len) / 2;
        r = l + best_len - 1;
        return best_len;
    }
};

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    static char s[MAXN + 5];
    if (scanf("%s", s) != 1) return 0;   // 读取一个单词

    Manacher man;
    man.build(s);

    int L, R;
    int len = man.longest(L, R);
    printf("%d\n", len);
    if (len > 0) printf("%.*s\n", len, s + L);
    return 0;
}
```

代码注释和实现符合竞赛风格：固定缓冲、快速 IO、清晰接口 `build` 与 `longest`，便于直接在比赛中使用或按需裁剪。

## 测试用例

输入：
```
abacaba
```

运行输出：
```
7
abacaba
```

解释：整个字符串本身是回文，长度为 7。

另一个例子：
输入：
```
banana
```
输出（示例）：
```
5
anana
```

## 经典例题

- 洛谷 P2049 / UVA / LeetCode 5 最长回文子串 — 直接使用 Manacher 可在 $O(n)$ 内完成。
- POJ 1961 / SPOJ 回文相关问题 — 在判断回文或计数时，Manacher 常用于优化。
- 区间与字符串查询混合题 — 将 Manacher 与后缀/哈希技术结合，解决更复杂的问题。

## 实践思考与扩展

- 若原串可能包含空格或需要读取整行，请使用 `fgets` 或 `getline`（替换 `scanf`）并去掉末尾换行。
- 若内存受限，可根据最大输入长度调整 `MAXN`。
- Manacher 能与哈希、后缀数组等工具配合，解决最长回文对、回文计数等变体问题。

## 参考

- Manacher, G. A linear-time algorithm for finding maximal palindromes in strings.
- 各大算法竞赛资料与题解（LeetCode 5, 洛谷 P2049 等）。


## 题目

- [[problem: luogu,P3805]]
- ❤️ 理解manacher算法,可以用出的单调队列优化,进一步可以进行线性DP [[problem: luogu,P4555]]
- [[problem: luogu,P1659]]