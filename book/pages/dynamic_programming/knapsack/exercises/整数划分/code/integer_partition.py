# 整数划分计数：dp[s] = 把 s 写成若干正整数之和（顺序不计）的方案数，模 1e9+7。
# 输入：一行一个整数 n（1 <= n <= 1000）。输出：dp[n] 对 1e9+7 取模。
# 内层 s 必须正序：同一部分 x 可以被重复使用，这正是完全背包的“无限件”写法。
import sys

MOD = 1_000_000_007


def main() -> None:
    data = iter(map(int, sys.stdin.buffer.read().split()))
    n = next(data)

    dp = [0] * (n + 1)
    dp[0] = 1  # 空划分是 0 的唯一方案

    # 按部分大小 x 从小到大处理，保证每种划分只按非降序被数一次（不重不漏）。
    for x in range(1, n + 1):
        for s in range(x, n + 1):
            dp[s] += dp[s - x]
            if dp[s] >= MOD:  # 加一次最多超 MOD 一倍，减一次即可
                dp[s] -= MOD

    print(dp[n])


if __name__ == "__main__":
    main()
