---
oj: "POJ"
problem_id: "1521"
title: "Entropy"
date: 2025-12-31 15:17
toc: true
tags: ["哈夫曼编码"]
desc: "哈夫曼编码模板题"
source: https://vjudge.net/problem/POJ-1521#author=DeepSeek_zh
---

[[TOC]]

## 题目解析


这是一个非常经典的算法题目，源自 **POJ 1521 - Entropy**。

题目的核心本质是要求你实现 **哈夫曼编码（Huffman Coding）** 算法，并将其压缩效果与定长的 8位 ASCII 编码进行对比。


## 1. 题目核心概念解析

- **原始编码（ASCII）**：固定长度编码。无论字符出现的频率如何，每个字符都占用 **8 bits**。
  - 计算公式：$Original\_Size = \text{字符串长度} \times 8$。
- **最优无前缀变长编码（Entropy/Huffman）**：根据字符出现的频率构建编码。
  - 频率高的字符用短的比特串表示。
  - 频率低的字符用长的比特串表示。
  - **无前缀（Prefix-free）**：没有任何一个字符的编码是另一个字符编码的前缀（例如，如果 A 是 `0`，那么 B 不可能是 `01`，否则解码会有歧义）。这正是哈夫曼树的特性。

## 2. 算法思路：哈夫曼树（Huffman Tree）

要计算最优编码的总长度，我们不需要真正生成每个字符的编码（如 "00", "01"），只需要计算**带权路径长度之和（WPL）**。

**具体步骤：**

1. **统计频率**：遍历输入字符串，统计每个字符（包括下划线）出现的次数。
2. **构建优先队列**：将所有出现过的字符的频率放入一个**小顶堆**（Min-Priority Queue）。
3. **贪心策略构建树**：
   - 从小顶堆中取出两个最小的频率值，设为 $a$ 和 $b$。
   - 将它们合并，产生一个新的节点，频率为 $a + b$。
   - 这个 $a + b$ 就是这两个节点在树中产生的“代价”（即编码长度贡献）。
   - 将 $a + b$ 重新放入堆中。
   - **累加代价**：将 $a + b$ 加到总的压缩位长度变量中。
   - 重复上述步骤，直到堆中只剩下一个节点。
4. **特殊情况处理**：
   - 如果字符串中只有一种字符（例如 "AAAAA"），哈夫曼树逻辑会直接结束。但在这种情况下，我们需要 1 个比特来表示该字符。
   - 所以，如果字符种类数为 1，压缩后的长度 = 字符串长度 $\times$ 1。

## 3. 样例分析

以输入 `AAAAABCD` 为例：

1. **统计**：A:5, B:1, C:1, D:1。
2. **堆**：`{1, 1, 1, 5}`。
3. **合并 1**：取出两个最小的 `1` (B) 和 `1` (C)。合并得 `2`。
   - 当前代价累加：2。
   - 堆变成：`{1, 2, 5}` （剩下的D和新合成的BC）。
4. **合并 2**：取出 `1` (D) 和 `2` (BC)。合并得 `3`。
   - 当前代价累加：$2 + 3 = 5$。
   - 堆变成：`{3, 5}`。
5. **合并 3**：取出 `3` 和 `5` (A)。合并得 `8`。
   - 当前代价累加：$5 + 8 = 13$。
   - 堆变成：`{8}`。停止。
6. **结果**：
   - ASCII长度：$8 \times 8 = 64$。
   - 哈夫曼长度：13。
   - 比率：$64 / 13 \approx 4.9$。

## 4. C++ 代码实现

C++

```
#include <iostream>
#include <string>
#include <queue>
#include <map>
#include <vector>
#include <cstdio> // 用于 printf

using namespace std;

int main() {
    string s;
    while (cin >> s && s != "END") {
        map<char, int> freq;
        // 1. 统计字符频率
        for (char c : s) {
            freq[c]++;
        }

        // 2. 将频率放入小顶堆
        // priority_queue 默认是大顶堆，使用 greater 变成小顶堆
        priority_queue<int, vector<int>, greater<int>> pq;
        
        for (auto const& [key, val] : freq) {
            pq.push(val);
        }

        long long original_len = s.length() * 8;
        long long compressed_len = 0;

        // 3. 特殊情况：只有一种字符
        if (pq.size() == 1) {
            compressed_len = s.length(); // 每个字符用1位表示
        } else {
            // 4. 哈夫曼树构建过程
            while (pq.size() > 1) {
                int a = pq.top(); pq.pop();
                int b = pq.top(); pq.pop();
                
                int combined = a + b;
                compressed_len += combined; // 累加代价
                
                pq.push(combined);
            }
        }

        // 5. 输出结果
        // %.1f 保留一位小数
        printf("%lld %lld %.1f\n", original_len, compressed_len, (double)original_len / compressed_len);
    }
    return 0;
}
```

## 5. 代码关键点说明

1. **数据类型**：虽然题目描述的字符串长度通常不会溢出 `int`，但在累加过程中使用 `long long` 是个好习惯，防止溢出。
2. **小顶堆**：`priority_queue<int, vector<int>, greater<int>>` 是 C++ STL 实现哈夫曼编码最方便的数据结构。它能保证每次 `pop()` 出来的都是当前频率最小的两个节点。
3. **计算逻辑**：
   - 传统的哈夫曼编码计算通常是：$\sum (\text{频率} \times \text{深度})$。
   - 代码中的简便算法利用了一个数学性质：**非叶子节点的权值之和等于所有叶子节点的带权路径长度之和**。每次合并操作 `a+b` 实际上就是生成了一个内部节点，所以直接累加 `a+b` 即可得到最终总长度。
4. **Corner Case (边界情况)**：
   - 务必注意 `if (pq.size() == 1)` 的判断。如果没有这个判断，`while (pq.size() > 1)` 循环一次都不会执行，导致 `compressed_len` 为 0，最终计算比率时会出现除以零错误（Runtime Error 或 NaN）。

## 6. 复杂度分析

- **时间复杂度**：
  - 统计频率：$O(N)$，其中 $N$ 是字符串长度。
  - 堆操作：假设字符集大小为 $C$（这里是大写字母+数字+下划线，最多约 40 种）。构建堆和循环合并的复杂度为 $O(C \log C)$。
  - 由于 $C$ 很小，整体复杂度主要取决于字符串长度，即 $O(N)$。
- **空间复杂度**：$O(C)$，用于存储字符频率映射和优先队列。
