# 完全背包二维 DP：dp[i][c] = 前 i 种物品每种可重复选、容量不超过 c 的最大价值。
# 输入：第一行 n capacity；随后 n 行每行 weight value（物品 1 下标，n 可为 0）。
# 输出：dp[n][capacity]。与 0/1 背包的唯一差别是转移来源为同一行 dp[i][c - wi]。
import sys


def main() -> None:
    data = iter(map(int, sys.stdin.buffer.read().split()))
    n = next(data)
    capacity = next(data)

    weight = [0] * (n + 1)
    value = [0] * (n + 1)
    for i in range(1, n + 1):
        weight[i] = next(data)
        value[i] = next(data)

    dp = [[0] * (capacity + 1) for _ in range(n + 1)]

    for i in range(1, n + 1):
        row = dp[i]
        prev = dp[i - 1]
        wi = weight[i]
        vi = value[i]
        for c in range(capacity + 1):
            best = prev[c]  # 完全不使用第 i 种物品
            if c >= wi:
                cand = row[c - wi] + vi  # 再用一件：仍在第 i 行上取，允许无限重复
                if cand > best:
                    best = cand
            row[c] = best

    print(dp[n][capacity])


if __name__ == "__main__":
    main()
