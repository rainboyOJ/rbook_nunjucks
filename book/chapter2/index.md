---
title: 第二章 - 高级算法主题
---

# 第二章：高级算法主题

## 2.1 动态规划（Dynamic Programming）

动态规划是一种通过将复杂问题分解为更小的子问题来解决问题的方法。与分治法不同，动态规划会存储子问题的解，避免重复计算。

### 基本思想

动态规划的核心思想是：**将问题分解为重叠子问题，存储子问题的解，避免重复计算**。

### 经典示例：斐波那契数列

```javascript
// 递归版本（效率低）
function fibonacci(n) {
    if (n <= 1) return n;
    return fibonacci(n - 1) + fibonacci(n - 2);
}

// 动态规划版本（效率高）
function fibonacciDP(n) {
    if (n <= 1) return n;
    
    const dp = new Array(n + 1);
    dp[0] = 0;
    dp[1] = 1;
    
    for (let i = 2; i <= n; i++) {
        dp[i] = dp[i - 1] + dp[i - 2];
    }
    
    return dp[n];
}

// 优化空间复杂度版本
function fibonacciOptimized(n) {
    if (n <= 1) return n;
    
    let prev = 0, curr = 1;
    for (let i = 2; i <= n; i++) {
        [prev, curr] = [curr, prev + curr];
    }
    
    return curr;
}
```

### 经典问题：最长公共子序列（LCS）

```javascript
function longestCommonSubsequence(text1, text2) {
    const m = text1.length, n = text2.length;
    const dp = Array(m + 1).fill(null).map(() => Array(n + 1).fill(0));
    
    for (let i = 1; i <= m; i++) {
        for (let j = 1; j <= n; j++) {
            if (text1[i - 1] === text2[j - 1]) {
                dp[i][j] = dp[i - 1][j - 1] + 1;
            } else {
                dp[i][j] = Math.max(dp[i - 1][j], dp[i][j - 1]);
            }
        }
    }
    
    return dp[m][n];
}
```

### 经典问题：0-1背包问题

```javascript
function knapsack(weights, values, capacity) {
    const n = weights.length;
    const dp = Array(n + 1).fill(null).map(() => Array(capacity + 1).fill(0));
    
    for (let i = 1; i <= n; i++) {
        for (let w = 1; w <= capacity; w++) {
            if (weights[i - 1] <= w) {
                dp[i][w] = Math.max(
                    dp[i - 1][w],
                    dp[i - 1][w - weights[i - 1]] + values[i - 1]
                );
            } else {
                dp[i][w] = dp[i - 1][w];
            }
        }
    }
    
    return dp[n][capacity];
}
```

## 2.2 贪心算法（Greedy Algorithm）

贪心算法在每一步都选择当前看起来最优的解，希望通过局部最优达到全局最优。

### 基本思想

贪心算法的核心思想是：**在每一步都做出当前看起来最好的选择，不考虑未来的影响**。

### 适用条件

1. **最优子结构**：问题的最优解包含子问题的最优解
2. **贪心选择性质**：全局最优解可以通过一系列局部最优选择得到

### 经典示例：活动选择问题

```javascript
function activitySelection(activities) {
    // 按结束时间排序
    activities.sort((a, b) => a.end - b.end);
    
    const selected = [activities[0]];
    let lastSelected = 0;
    
    for (let i = 1; i < activities.length; i++) {
        if (activities[i].start >= activities[lastSelected].end) {
            selected.push(activities[i]);
            lastSelected = i;
        }
    }
    
    return selected;
}

// 使用示例
const activities = [
    { start: 1, end: 3, name: '活动A' },
    { start: 3, end: 5, name: '活动B' },
    { start: 0, end: 6, name: '活动C' },
    { start: 5, end: 7, name: '活动D' },
    { start: 8, end: 9, name: '活动E' },
    { start: 5, end: 9, name: '活动F' }
];

console.log(activitySelection(activities));
```

### 经典示例：哈夫曼编码

```javascript
class HuffmanNode {
    constructor(char, freq) {
        this.char = char;
        this.freq = freq;
        this.left = null;
        this.right = null;
    }
}

function buildHuffmanTree(frequencies) {
    const priorityQueue = [];
    
    // 创建叶子节点
    for (const [char, freq] of Object.entries(frequencies)) {
        priorityQueue.push(new HuffmanNode(char, freq));
    }
    
    // 按频率排序
    priorityQueue.sort((a, b) => a.freq - b.freq);
    
    // 构建哈夫曼树
    while (priorityQueue.length > 1) {
        const left = priorityQueue.shift();
        const right = priorityQueue.shift();
        
        const merged = new HuffmanNode(null, left.freq + right.freq);
        merged.left = left;
        merged.right = right;
        
        priorityQueue.push(merged);
        priorityQueue.sort((a, b) => a.freq - b.freq);
    }
    
    return priorityQueue[0];
}

function generateHuffmanCodes(root, code = '', codes = {}) {
    if (!root) return codes;
    
    if (root.char !== null) {
        codes[root.char] = code || '0';
    }
    
    generateHuffmanCodes(root.left, code + '0', codes);
    generateHuffmanCodes(root.right, code + '1', codes);
    
    return codes;
}
```

### 经典示例：最小生成树（Kruskal算法）

```javascript
class UnionFind {
    constructor(n) {
        this.parent = Array(n).fill(0).map((_, i) => i);
        this.rank = Array(n).fill(0);
    }
    
    find(x) {
        if (this.parent[x] !== x) {
            this.parent[x] = this.find(this.parent[x]);
        }
        return this.parent[x];
    }
    
    union(x, y) {
        const px = this.find(x);
        const py = this.find(y);
        
        if (px === py) return false;
        
        if (this.rank[px] < this.rank[py]) {
            this.parent[px] = py;
        } else if (this.rank[px] > this.rank[py]) {
            this.parent[py] = px;
        } else {
            this.parent[py] = px;
            this.rank[px]++;
        }
        
        return true;
    }
}

function kruskal(edges, n) {
    // 按权重排序
    edges.sort((a, b) => a[2] - b[2]);
    
    const uf = new UnionFind(n);
    const mst = [];
    let totalWeight = 0;
    
    for (const [u, v, weight] of edges) {
        if (uf.union(u, v)) {
            mst.push([u, v, weight]);
            totalWeight += weight;
        }
    }
    
    return { mst, totalWeight };
}
```

## 2.3 动态规划 vs 贪心算法

| 特性 | 动态规划 | 贪心算法 |
|------|----------|----------|
| 最优解 | 保证全局最优 | 不一定保证全局最优 |
| 时间复杂度 | 通常较高 | 通常较低 |
| 空间复杂度 | 通常需要额外空间 | 通常空间效率更高 |
| 适用性 | 更广泛 | 特定问题 |
| 决策方式 | 考虑所有子问题 | 只做局部最优选择 |

### 选择指南

1. **使用动态规划**：
   - 问题具有最优子结构
   - 需要保证全局最优解
   - 子问题有重叠

2. **使用贪心算法**：
   - 问题具有贪心选择性质
   - 局部最优能导致全局最优
   - 需要高效的解决方案

## 2.4 实战应用

### 股票买卖问题（动态规划）

```javascript
function maxProfit(prices) {
    if (prices.length < 2) return 0;
    
    let minPrice = prices[0];
    let maxProfit = 0;
    
    for (let i = 1; i < prices.length; i++) {
        maxProfit = Math.max(maxProfit, prices[i] - minPrice);
        minPrice = Math.min(minPrice, prices[i]);
    }
    
    return maxProfit;
}
```

### 硬币找零问题（动态规划）

```javascript
function coinChange(coins, amount) {
    const dp = Array(amount + 1).fill(Infinity);
    dp[0] = 0;
    
    for (let i = 1; i <= amount; i++) {
        for (const coin of coins) {
            if (coin <= i) {
                dp[i] = Math.min(dp[i], dp[i - coin] + 1);
            }
        }
    }
    
    return dp[amount] === Infinity ? -1 : dp[amount];
}
```

## 2.5 小结

本章深入探讨了动态规划和贪心算法这两种重要的算法设计技术：

- **动态规划**：通过存储子问题的解来避免重复计算，适用于具有最优子结构和重叠子问题的问题
- **贪心算法**：通过局部最优选择来达到全局最优，适用于具有贪心选择性质的问题

### 学习建议

1. 理解两种算法的核心思想和适用条件
2. 通过大量练习掌握经典问题的解法
3. 学会分析问题的性质，选择合适的算法
4. 注意时间复杂度和空间复杂度的权衡

---

> **练习建议**：
> - 实现不同版本的斐波那契数列计算，比较性能差异
> - 解决经典的背包问题变种
> - 实现活动选择问题的贪心算法
> - 尝试用动态规划解决贪心算法的问题，验证结果的最优性