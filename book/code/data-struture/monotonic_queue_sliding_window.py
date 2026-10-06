# 单调队列求滑动窗口最值：输出每个长度为 k 的窗口的最小值与最大值。
# a 为 1 下标（a[0] 占位不用），窗口数 n-k+1，要求 1 <= k <= n。
# 队列存下标且对应值单调：求最小用递增队列（队首最小），求最大用递减队列（队首最大）。
# 用 collections.deque 而非 list，保证队首删除是 O(1)。
# 调用示例：sliding_window_min_max([0, 3, 1, 4], 2) -> ([1, 1, 1], [3, 4, 4])

from collections import deque

type Arr = list[int]  # 1 下标数组，a[0] 占位不用


def sliding_window_min_max(a: Arr, k: int) -> tuple[Arr, Arr]:
    """返回 (每个窗口的最小值, 每个窗口的最大值)，两个列表长度都是 n-k+1。"""
    n = len(a) - 1
    mins: Arr = []
    maxs: Arr = []

    q: deque[int] = deque()
    for i in range(1, n + 1):
        # 队首下标滑出窗口 [i-k+1, i] 就弹出。
        while q and q[0] <= i - k:
            q.popleft()
        # 队尾对应值 >= a[i] 的永远不可能成为后面窗口的最小值，弹出。
        while q and a[q[-1]] >= a[i]:
            q.pop()
        q.append(i)
        if i >= k:
            mins.append(a[q[0]])

    q.clear()
    for i in range(1, n + 1):
        while q and q[0] <= i - k:
            q.popleft()
        # 求最大值时反向：队尾对应值 <= a[i] 的弹出。
        while q and a[q[-1]] <= a[i]:
            q.pop()
        q.append(i)
        if i >= k:
            maxs.append(a[q[0]])

    return mins, maxs
