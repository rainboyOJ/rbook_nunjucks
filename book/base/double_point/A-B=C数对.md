给一个数组,和数字$C$
求数组中$A-B=C$的对数

暴力$n^2$


## 二分
1. 排序后不影响结果,先排序
2. 固定一个数$A$,然后二分查找$B = A - C$的数量
3. $O(nlogn)$

在一个不没有重复元素的数组中,求数组中$A-B=C$的对数

这是一个经典的\*\*“同向双指针”\*\*（Same-direction Two Pointers）问题。

与“两数之和”（Two Sum）不同，“两数之和”通常指针分别在头尾向中间靠拢；而“两数之差”需要两个指针**都从左边出发，向右移动**。

### 核心思路

1.  **排序**：
    双指针法依赖于数据的单调性。原数组无序时，我们无法判断指针该往哪边移。
    首先将数组从小到大排序。

2.  **指针定义**：

      * `left` 指针指向 $B$（较小的数）。
      * `right` 指针指向 $A$（较大的数）。
      * 我们要寻找满足 `nums[right] - nums[left] == C` 的情况。

3.  **移动策略**：
    计算当前差值 `diff = nums[right] - nums[left]`：

      * **如果 `diff < C`**：说明差太小了。因为 `left` 已经是最小能取的了，我们必须让被减数 $A$ 变大，所以 **`right` 向右移**。
      * **如果 `diff > C`**：说明差太大了。我们要让减数 $B$ 变大（从而减小差值），所以 **`left` 向右移**。
      * **如果 `diff == C`**：找到了！计数器 `count++`。
          * 由于题目保证**没有重复元素**，这对 $(A, B)$ 组合是唯一的。
          * 我们可以同时移动 `left++` 和 `right++` 来寻找下一对。

4.  **特殊处理**：

      * 如果 $C < 0$：由于题目求 $A-B=C$，即 $B-A = -C$。对数是一样的，取 $C = |C|$ 处理即可。
      * 为了避免 `left` 和 `right` 指向同一个元素（差为0，但题目通常隐含 $A, B$ 是不同元素，且若 $C>0$ 则不可能相同），通常初始化 `right = 1` 或者在循环中保证 `right > left`。

-----

### 代码实现

#### Python 实现

```python
from typing import List

def count_diff_pairs(nums: List[int], c: int) -> int:
    # 1. 预处理：处理负数 C，并将数组排序
    c = abs(c) 
    nums.sort()
    
    n = len(nums)
    left = 0
    right = 1 # 让 right 从第二个元素开始
    count = 0
    
    while right < n:
        # 边界保护：确保 left 永远不追上 right（针对 C=0 的情况）
        # 如果 left == right，说明没有间隔，强制 right 走一步
        if left == right:
            right += 1
            continue
            
        diff = nums[right] - nums[left]
        
        if diff == c:
            count += 1
            # 题目承诺无重复元素，所以这一对用完就可以扔了
            left += 1
            right += 1
        elif diff < c:
            # 差太小，增大被减数
            right += 1
        else:
            # 差太大，增大减数（即减小差值）
            left += 1
            
    return count

# 测试
# 排序后: [1, 2, 3, 5, 8], C=3
# 5-2=3, 8-5=3 -> 2对
arr = [8, 5, 1, 3, 2]
target = 3
print(count_diff_pairs(arr, target)) # 输出: 2
```

#### C++ 实现

```cpp
#include <vector>
#include <algorithm>
#include <iostream>
#include <cmath>

using namespace std;

int countDiffPairs(vector<int>& nums, int c) {
    c = abs(c); // 统一转为正数处理
    sort(nums.begin(), nums.end()); // 1. 排序 O(N log N)
    
    int n = nums.size();
    int left = 0;
    int right = 1;
    int count = 0;
    
    while (right < n) {
        if (left == right) {
            right++;
            continue;
        }
        
        long long diff = (long long)nums[right] - nums[left];
        
        if (diff == c) {
            count++;
            left++;
            right++; // 无重复元素，可以大胆同时移
        } else if (diff < c) {
            right++; // 差小了，变大 A
        } else {
            left++;  // 差大了，变大 B
        }
    }
    
    return count;
}
```

-----

### 复杂度分析

1.  **时间复杂度**: $O(N \log N)$

      * **排序** 消耗 $O(N \log N)$。
      * **双指针遍历** 消耗 $O(N)$。因为 `left` 和 `right` 都只会从头走到尾，互不回头。
      * 总复杂度由排序主导。

2.  **空间复杂度**: $O(1)$ (不计算存储输入输出的空间，仅看额外变量)。

      * 如果是 Python 的 `sort` (Timsort) 可能需要 $O(N)$ 的栈空间。C++ `std::sort` 通常是 $O(\log N)$。

### 对比：如果有重复元素怎么办？

如果题目去掉了“无重复元素”的条件（例如 `[1, 1, 4, 4]`, $C=3$），代码会变得麻烦：

  * 当 `diff == C` 时，我们不能简单地 `left++, right++`。
  * 我们需要计算 `left` 指向的数字有几个（比如 $x$ 个），`right` 指向的数字有几个（比如 $y$ 个）。
  * 这一轮的贡献是 $x \times y$。
  * 然后指针跳过所有相同的元素。
  * **本题因为“无重复”这一条件，极大地简化了逻辑。**


## 证明

为了证明这个双指针算法的正确性，我们利用**“单调性”**和**“解空间覆盖”**的思路。

我们要证明的核心是：**算法绝对不会错过任何一对满足条件的 $(A, B)$。**

假设数组 $Nums$ 已经升序排列，目标对是 $(i, j)$，即 $Nums[j] - Nums[i] = C$ （其中 $i < j$）。

---

### 证明思路：反证法 (Proof by Contradiction)

我们假设存在一对标准答案 $(i, j)$，但是算法运行结束时**没有找到它**。

**分析算法流程：**
右指针 $R$ 从左向右一步步走，**必然**会在某个时刻到达位置 $j$。
我们要观察：**当 $R$ 刚刚到达 $j$ 这一瞬间，左指针 $L$ 在哪里？**

只有三种情况：

#### 情况 1：$L$ 还在 $i$ 的左边 ($L < i$)
* **现状**：$R = j$, $L < i$。
* **推导**：
    由于数组单调递增，$L < i \implies Nums[L] < Nums[i]$。
    计算差值：$Diff = Nums[j] - Nums[L]$。
    因为 $Nums[L]$ 比标准答案 $Nums[i]$ 小，所以 **$Diff$ 必定大于 $C$**。
* **算法行为**：根据算法逻辑，当 `Diff > C` 时，执行 `L++`。
* **结果**：$L$ 会一直向右移动，直到 $L$ 变成 $i$。此时 $Diff$ 变成 $C$，答案被找到。
* **结论**：如果 $L$ 落后了，它会被强制追上来，**不会漏**。

#### 情况 2：$L$ 正好在 $i$ 的位置 ($L = i$)
* **现状**：$R = j$, $L = i$。
* **推导**：$Diff = Nums[j] - Nums[i] = C$。
* **结果**：直接命中，计数 `count++`。
* **结论**：**没有漏**。

#### 情况 3：$L$ 已经跑到了 $i$ 的右边 ($L > i$) —— **这是唯一需要反驳的“漏网之鱼”情况**
* **假设**：当 $R$ 刚走到 $j$ 时，$L$ 却早已越过了 $i$。
* **回溯推理**：
    * $L$ 要越过 $i$（从 $i$ 变成 $i+1$），必然是在之前的某个时刻发生的。
    * 让时间倒流回 **$L$ 停留在 $i$ 的那个时刻**。
    * 在那一刻，右指针 $R$ 一定在 $j$ 的左边某个位置，设为 $k$ ($k < j$)。
    * **当时为何 $L$ 会移动？** 算法规定只有当 `Nums[R] - Nums[L] > C` 时，$L$ 才会右移。
    * 所以，当时必然满足：$Nums[k] - Nums[i] > C$。
* **寻找矛盾**：
    * 已知 $k < j$ (右指针还没走到 $j$)，根据数组升序，则 $Nums[k] < Nums[j]$。
    * 那么：$Nums[k] - Nums[i] \quad < \quad Nums[j] - Nums[i]$。
    * 我们已知 $Nums[j] - Nums[i] = C$。
    * 所以推出：$Nums[k] - Nums[i] < C$。
* **矛盾点**：
    * 算法逻辑要求：只有差值 **大于** $C$ 才会移走 $L$。
    * 数学事实推导：此时差值 **小于** $C$。
    * **结论**：在 $R$ 到达 $j$ 之前，$L$ **绝对不可能**因为差值过大而提前移过 $i$。

### 总结

1.  **右指针**负责地毯式搜索，一定会经过 $j$。
2.  **左指针**受到数学逻辑的严格约束：
    * 只要 $R$ 还没到 $j$，差值就不够大，$L$ 就不敢越过 $i$。
    * 一旦 $R$ 到了 $j$，如果 $L$ 还在后面，差值就太大，$L$ 被迫赶去 $i$。
3.  **因此，两个指针必在 $(i, j)$ 处相遇。**
   
这个证明展示了算法的严谨性，确保了它不会漏掉任何一对满足条件的 $(A, B)$。


## luogu P1102


```cpp
#include <bits/stdc++.h>
using namespace std;
const long long maxn = 1e6+5;
long long a[maxn];
long long b[maxn];
long long cnt[maxn];
long long n,c;
int main(long long argc, char const *argv[])
{
  cin >> n >> c;
  for (long long i = 0; i < n; i++)
  {
    cin >> a[i+1];
  }
  sort(a+1,a+1+n);
  b[1] = a[1];
  cnt[1]++;
  long long idx = 1;
  for(long long i = 2;i<=n;i++) {
    if( a[i] == a[i-1]) {
      cnt[idx]++;
    }
    else {
      idx++;
      cnt[idx] = 1;
      b[idx] = a[i];
    }
  }
  // for(long long i =1;i<=idx;i++)
  // {
  //   cout << b[i] << ' ' << cnt[i] << endl;
  // }

  long long h=1,t=2;

  long long ans = 0;
  while(1) {

    if( t > idx || h > idx ) break;

    if( b[t] - b[h] == c) {
      ans += cnt[t] * cnt[h];
      // b[t]不可能再和前面产生贡献了
      t++;
    }
    else if (b[t] - b[h] > c) {
      h++;
    }
    else if ( b[t] - b[h] < c) {
      t++;
    }
  }
  cout << ans << endl;
  
  
  return 0;
}

```