# 状压 DP 求 TSP 最短哈密顿路径：从 0 号点出发、每个点恰好访问一次的最小总代价。
# cost 为 n×n 邻接矩阵（cost[i][j] 是 i→j 的代价，可用 INF 表示不可达），要求 n >= 1。
# C++ 的 long long 与 INF = 1LL << 60 在 Python 里是任意精度 int，不会溢出。

INF = 1 << 60  # 哨兵，约 1.15e18，比任何合法路径代价都大

type Matrix = list[list[int]]  # 二维表：cost 为 n×n，dp 为 2^n 行 × n 列


def bitmask_dp_tsp(cost: Matrix) -> int:
    """返回从 0 出发访问全部点的最小路径代价；图不连通时返回 INF。"""
    n = len(cost)
    limit = 1 << n  # 状态总数 2^n，mask 第 i 位为 1 表示点 i 已访问
    dp: Matrix = [[INF] * n for _ in range(limit)]
    dp[1][0] = 0  # 只访问点 0 且停在点 0：1 == 1 << 0，代价为 0

    # mask 从小到大枚举：new_mask = mask | (1 << nxt) 恒大于 mask，故无后效性。
    for mask in range(1, limit):
        for last in range(n):
            if dp[mask][last] == INF:
                continue
            if (mask & (1 << last)) == 0:
                continue  # last 必须真在 mask 中，否则该状态本身无意义
            for nxt in range(n):
                if mask & (1 << nxt):
                    continue  # nxt 已访问过，不能再走
                new_mask = mask | (1 << nxt)
                cand = dp[mask][last] + cost[last][nxt]
                if cand < dp[new_mask][nxt]:
                    dp[new_mask][nxt] = cand

    full = limit - 1  # 全 1 掩码，表示所有点都访问过
    answer = INF
    for last in range(n):
        if dp[full][last] < answer:
            answer = dp[full][last]
    return answer
