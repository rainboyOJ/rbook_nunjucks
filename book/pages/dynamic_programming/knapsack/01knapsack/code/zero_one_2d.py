# 0/1 背包二维 DP：dp[i][c] = 只从前 i 件物品里选、容量不超过 c 的最大价值。
# 输入：第一行 n capacity；随后 n 行每行 weight value（物品 1 下标，n 可为 0）。
# 输出：dp[n][capacity]。价值可能为负，故最优解允许一件都不选（答案为 0 起步）。
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

    # 保留整张表而不是滚动数组：dp[i] 的语义就是“前 i 件”，便于和转移式一一对应。
    dp = [[0] * (capacity + 1) for _ in range(n + 1)]

    for i in range(1, n + 1):
        row = dp[i]
        prev = dp[i - 1]
        wi = weight[i]
        vi = value[i]
        for c in range(capacity + 1):
            best = prev[c]  # 不选第 i 件
            if c >= wi:
                cand = prev[c - wi] + vi  # 选第 i 件，容量回退到 i-1 行
                if cand > best:
                    best = cand
            row[c] = best

    print(dp[n][capacity])


if __name__ == "__main__":
    main()
