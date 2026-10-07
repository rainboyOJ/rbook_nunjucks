# 数字三角形最大路径和：从顶出发，每步只能走向左下方或右下方，直到最底行。
# 输入：第一行 n；随后第 i 行有 i 个整数（1 <= i <= n）。数字可为负。
# 输出：最大路径和。dp 从底向上倒推，原地更新 dp[j] 即“从第 i 行第 j 列到底部的最大和”。
import sys


def main() -> None:
    data = iter(map(int, sys.stdin.buffer.read().split()))
    n = next(data)

    a = [[0] * (n + 2) for _ in range(n + 1)]
    for i in range(1, n + 1):
        for j in range(1, i + 1):
            a[i][j] = next(data)

    dp = [0] * (n + 2)
    for j in range(1, n + 1):
        dp[j] = a[n][j]  # 最底行本身就是终点

    # 倒推：第 i 行的 dp[j] 只依赖下一行的 dp[j] 与 dp[j + 1]，
    # 因为 dp[j] 先于 dp[j + 1] 更新，用同一行原地覆盖不会读到被污染的值。
    for i in range(n - 1, 0, -1):
        for j in range(1, i + 1):
            dp[j] = max(dp[j], dp[j + 1]) + a[i][j]

    print(dp[1])


if __name__ == "__main__":
    main()
