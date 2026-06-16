---
title: "滑动窗口"
date: 2025-10-30 14:19
toc: true
tags: [""]
categories: [""]
---

[[TOC]]



## 引入题目

长度为k的滑动窗口, 从左到右滑动, 每次滑动一格, 计算窗口内的和, 记录最大值

## 解决思路

1. 穷举法
2. 滑动窗口法

## 穷举法

最直接的方法是穷举所有可能的窗口。一个窗口由它的起始位置决定。第一个窗口从索引0开始，最后一个窗口从索引`n-k`开始。对于每个窗口，我们计算其内部所有元素的和，并记录下遇到的最大和。

下面是一个C++的实现示例：

```cpp
#include <iostream>
#include <vector>
#include <numeric>
#include <algorithm>

int max_sum_brute_force(const std::vector<int>& nums, int k) {
    if (nums.size() < k) {
        return 0; // 或者进行错误处理
    }
    int max_sum = 0;
    // 为了正确初始化max_sum, 先计算第一个窗口的和
    for(int i = 0; i < k; ++i) {
        max_sum += nums[i];
    }

    // 从第二个窗口开始
    for (size_t i = 1; i <= nums.size() - k; ++i) {
        int current_sum = 0;
        for (size_t j = 0; j < k; ++j) {
            current_sum += nums[i + j];
        }
        if (current_sum > max_sum) {
            max_sum = current_sum;
        }
    }
    return max_sum;
}
```

这种方法的时间复杂度是 O((n-k+1) * k)，约等于 O(n*k)。当n和k很大时，这个方法会非常慢。

## 滑动窗口法

我们可以优化计算过程。我们不需要为每个窗口都重新计算总和，而是可以利用前一个窗口的计算结果。

当窗口向右滑动一个位置时，我们只需要从前一个窗口的和中减去离开窗口的元素（即前一个窗口的第一个元素），并加上进入窗口的元素（即新窗口的最后一个元素）。

1.  计算第一个窗口（索引从0到k-1）的和。这个和作为初始的`max_sum`和`current_sum`。
2.  从索引`k`开始遍历到`n-1`。在每次迭代中，我们滑动窗口：
    *   通过加上新元素`nums[i]`并减去旧元素`nums[i-k]`来更新`current_sum`。
    *   更新`max_sum = max(max_sum, current_sum)`。

下面是一个C++的实现示例：

```cpp
#include <iostream>
#include <vector>
#include <numeric>
#include <algorithm>

int max_sum_sliding_window(const std::vector<int>& nums, int k) {
    if (nums.size() < k) {
        return 0; // 或者进行错误处理
    }
    int current_sum = 0;
    // 计算第一个窗口的和
    for (int i = 0; i < k; ++i) {
        current_sum += nums[i];
    }

    int max_sum = current_sum;

    // 滑动窗口
    for (size_t i = k; i < nums.size(); ++i) {
        current_sum = current_sum - nums[i - k] + nums[i];
        if (current_sum > max_sum) {
            max_sum = current_sum;
        }
    }
    return max_sum;
}
```

这种方法的时间复杂度是O(n)，因为我们只需要遍历数组一次。这是一个显著的改进。

## 其它

### 1. 滑动窗口法的时间复杂度是多少?

时间复杂度是 **O(n)**。
我们首先用O(k)的时间计算第一个窗口的和。然后，我们遍历数组剩下的`n-k`个元素，对于每个元素，我们都执行固定数量的操作（一次加法，一次减法，一次比较）。所以总的时间复杂度是O(k + (n-k)) = O(n)。

### 2. 如何证明滑动窗口法的正确性?

该算法的正确性依赖于它能正确计算出每个大小为`k`的连续子数组的和，并找出其中的最大值。

*   **初始化:** 第一个窗口（索引从0到k-1）的和被正确计算。
*   **迭代 (归纳步骤):** 假设在第`i-1`步（`i >= k`），`current_sum`保存了从`i-k`到`i-1`的元素和。当我们移动到第`i`步时，我们想要计算从`i-k+1`到`i`的元素和。新的和是`sum(nums[i-k+1]...nums[i])`。这个和可以从前一个和`sum(nums[i-k]...nums[i-1])`通过减去`nums[i-k]`并加上`nums[i]`得到。这正是更新步骤`current_sum = current_sum - nums[i-k] + nums[i]`所做的。
*   **结论:** 由于该算法从左到右正确地计算了每个窗口的和，并持续跟踪遇到的最大和，因此它保证能找到任何k大小窗口的最大和。

## 滑动窗口与01序列

这个问题要求计算一个大小为k的滑动窗口在一个01序列上滑动时的`and`和`or`的值。

### `and` 运算

窗口中所有元素的`and`值为1，当且仅当窗口中所有元素都为1。我们可以通过计算当前窗口中0的数量来解决这个问题。

1.  初始化一个窗口，并计算其中0的数量`zeros_count`。
2.  如果`zeros_count`为0，则第一个窗口的结果为1，否则为0。
3.  滑动窗口：
    *   如果离开窗口的元素是0，则`zeros_count`减1。
    *   如果进入窗口的元素是0，则`zeros_count`加1。
    *   新窗口的结果是1（如果`zeros_count`为0），否则是0。

### `or` 运算

窗口中所有元素的`or`值为1，只要窗口中至少有一个1。这等价于说，结果为0当且仅当所有元素都为0。我们可以通过计算窗口中1的数量来解决。

1.  初始化一个窗口，并计算其中1的数量`ones_count`。
2.  如果`ones_count` > 0，则第一个窗口的结果为1，否则为0。
3.  滑动窗口：
    *   如果离开窗口的元素是1，则`ones_count`减1。
    *   如果进入窗口的元素是1，则`ones_count`加1。
    *   新窗口的结果是1（如果`ones_count` > 0），否则是0。

这两种运算都可以使用滑动窗口技术在O(n)时间内解决。
