# 单调队列 + 前缀和：求长度不超过 m 的最大子段和。
# a 为 1 下标（a[0] 占位不用），要求 n >= 1 且 m >= 1；此时答案一定存在（至少取一个元素）。
# prefix[i] = a[1] + ... + a[i]，子段 (j, i] 的和 = prefix[i] - prefix[j]。
# 固定右端 i 时，只需在 j ∈ [i-m, i-1] 里找最小的 prefix[j]，故用递增单调队列维护该窗口最小值。
# C++ 用 long long 防前缀和溢出 int；Python int 任意精度，该坑不存在。

from collections import deque

type Arr = list[int]  # 1 下标数组，a[0] / prefix[0] 占位不用


def max_subarray_sum(a: Arr, m: int) -> int:
    """返回长度 <= m 的非空子段最大和。调用示例：max_subarray_sum([0, 2, -1, 3], 2) -> 3"""
    n = len(a) - 1

    # prefix 同样 1 下标；prefix[0] = 0 作为空前缀哨兵。
    prefix: Arr = [0] * (n + 1)
    for i in range(1, n + 1):
        prefix[i] = prefix[i - 1] + a[i]

    # n >= 1 时子段 (0, 1] 一定合法（m >= 1），用它当初始答案，
    # 避免像 C++ 那样用 LLONG_MIN 哨兵（Python 无固定下界）。
    answer = prefix[1] - prefix[0]
    q: deque[int] = deque([0])

    for i in range(1, n + 1):
        # 合法左端点 j 需满足 i - j <= m，即 j >= i - m。
        while q and q[0] < i - m:
            q.popleft()

        best = prefix[i] - prefix[q[0]]
        if best > answer:
            answer = best

        # 队尾 prefix 不小于 prefix[i] 的，作为左端点不如 i 优，弹出。
        while q and prefix[q[-1]] >= prefix[i]:
            q.pop()
        q.append(i)

    return answer
