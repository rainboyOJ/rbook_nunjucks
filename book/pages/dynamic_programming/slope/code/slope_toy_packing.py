# 斜率优化 DP（玩具装箱）：dp[i] = min_{j<i} dp[j] + (x[i] - x[j] - L - 1)^2，x[i] = prefix[i] + i。
# 输入：第一行 n L；随后 n 个物品长度 c_i（n >= 1）。输出：dp[n]。
# 叉积比较全用 Python 任意精度整数，因此不需要 C++ 里的 __int128。
import sys
from collections import deque


def main() -> None:
    data = iter(map(int, sys.stdin.buffer.read().split()))
    n = next(data)
    L = next(data)

    prefix = [0] * (n + 1)
    for i in range(1, n + 1):
        prefix[i] = prefix[i - 1] + next(data)

    # x[i] 是决策点的横坐标，因为 c_i >= 0 所以严格递增，保证凸壳斜率单调。
    x = [prefix[i] + i for i in range(n + 1)]
    dp = [0] * (n + 1)

    def b(i: int) -> int:
        return x[i] - L - 1

    def y(j: int) -> int:
        return dp[j] + x[j] * x[j]

    def value(j: int, i: int) -> int:
        # 把 (B_i - x[j])^2 展开后与 i 无关的截距部分：Y_j - 2 * B_i * X_j
        return y(j) - 2 * b(i) * x[j]

    def bad(a: int, mid: int, c: int) -> bool:
        # 叉积比较 slope(a, mid) >= slope(mid, c)，避免除法与浮点误差
        return (y(mid) - y(a)) * (x[c] - x[mid]) >= (y(c) - y(mid)) * (x[mid] - x[a])

    q = deque([0])  # 下凸壳，横坐标随入队递增；队首是当前最优决策点
    for i in range(1, n + 1):
        # 若第二个点已不劣于队首，由于 B_i 单调递增，队首以后也不会再变优
        while len(q) >= 2 and value(q[1], i) <= value(q[0], i):
            q.popleft()

        j = q[0]
        dp[i] = b(i) * b(i) + value(j, i)

        # 队尾三点若斜率不递增，则中间点永远不可能成为最优决策，弹出
        while len(q) >= 2 and bad(q[-2], q[-1], i):
            q.pop()
        q.append(i)

    print(dp[n])


if __name__ == "__main__":
    main()
