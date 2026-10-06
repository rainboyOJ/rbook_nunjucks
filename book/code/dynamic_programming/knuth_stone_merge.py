# 石子合并（Knuth 优化）：相邻两堆合并的代价为两堆之和，求合并成一堆的最小总代价。
# a 是 1 下标（a[0] 占位不用），n = len(a) - 1；dp[l][r] 为合并 a[l..r] 的最小代价。
# C++ 的 long long 与 INF = 1LL << 62 在 Python 里是任意精度 int，不会溢出。

INF = 1 << 62  # 哨兵，约 4.6e18，比任何合法合并总代价都大

type Table = list[list[int]]


def knuth_stone_merge(a: list[int]) -> int:
    """返回把 a[1..n] 合并成一堆的最小代价；n = 0 或 1 时为 0。"""
    n = len(a) - 1

    prefix = [0] * (n + 1)
    for i in range(1, n + 1):
        prefix[i] = prefix[i - 1] + a[i]

    def seg_sum(l: int, r: int) -> int:
        # 对应 C++ 的 lambda sum；这里改名以免遮蔽内置 sum。
        return prefix[r] - prefix[l - 1]

    dp: Table = [[0] * (n + 2) for _ in range(n + 2)]
    opt: Table = [[0] * (n + 2) for _ in range(n + 2)]
    for i in range(1, n + 1):
        opt[i][i] = i  # 单点区间的最优分割点就是它自己

    for length in range(2, n + 1):
        for l in range(1, n - length + 2):
            r = l + length - 1
            dp[l][r] = INF

            # Knuth 优化：opt[l][r-1] <= opt[l][r] <= opt[l+1][r]，
            # 把 k 的枚举范围收窄，总复杂度从 O(n^3) 降到 O(n^2)。
            left = max(opt[l][r - 1], l)
            right = min(opt[l + 1][r], r - 1)
            for k in range(left, right + 1):
                cur = dp[l][k] + dp[k + 1][r] + seg_sum(l, r)
                if cur < dp[l][r]:
                    dp[l][r] = cur
                    opt[l][r] = k

    return dp[1][n]
