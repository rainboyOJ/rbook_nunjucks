# 整数划分：f(n, m) 表示把 n 拆成不超过 m 的正整数之和的方案数。
# 递归深度约为 n + m（≤ 1000），main 中已抬高 sys.setrecursionlimit。
# 方案数增长很快，C++ 用 long long 也会溢出，Python int 为任意精度无此问题。
import sys


def main() -> None:
    data = sys.stdin.buffer.read().split()
    n = int(data[0])

    # memo[n][m]：-1 表示未计算（对应 C++ 的 memset(-1)）。
    memo = [[-1] * (n + 1) for _ in range(n + 1)]

    def f(n: int, m: int) -> int:
        if n == 0:
            return 1
        if m == 0:
            return 0
        if m > n:
            return f(n, n)
        if memo[n][m] != -1:
            return memo[n][m]
        # 不使用 m 的方案 + 至少使用一个 m 的方案。
        memo[n][m] = f(n, m - 1) + f(n - m, m)
        return memo[n][m]

    sys.setrecursionlimit(10000)
    print(f(n, n))


if __name__ == "__main__":
    main()
