# 01 背包「恰好装满」：求容量恰好为 capacity 时的最大价值，凑不出该容量则无解。
# dp[c] 表示容量恰好为 c 时的最大价值，用 NEG_INF 标记「凑不出」；容量 0 下标。
# C++ 的 const int NEG_INF = -1e9 与 int 累加溢出在 Python 里都不存在（任意精度 int）。

NEG_INF = -10**9  # 哨兵：对应 C++ 的 const int NEG_INF = -1e9

type Item = tuple[int, int]  # (weight, value)
type Items = list[Item]


def zero_one_exact_fill(capacity: int, items: Items) -> int | None:
    """返回容量恰好为 capacity 时的最大价值；无法恰好装满时返回 None。"""
    dp = [NEG_INF] * (capacity + 1)
    dp[0] = 0  # 容量 0 恒可凑出，价值为 0

    for weight, value in items:
        # 01 背包必须倒序枚举容量，避免同一物品被重复使用。
        for c in range(capacity, weight - 1, -1):
            if dp[c - weight] == NEG_INF:
                continue  # 前驱容量凑不出，不能从这个状态转移
            cand = dp[c - weight] + value
            if cand > dp[c]:
                dp[c] = cand

    if dp[capacity] == NEG_INF:
        return None  # C++ 版此处打印 "Impossible"
    return dp[capacity]
