# 完全背包一维优化：每种物品可无限次取用，求容量 capacity 内的最大价值。
# items 为 (weight, value) 列表，容量用 0 下标，dp[c] 表示容量不超过 c 的最大价值。
# C++ 用 int 存价值，累加可能溢出 int；Python int 任意精度，该坑不存在。

type Item = tuple[int, int]  # (weight, value)
type Items = list[Item]


def complete_1d(capacity: int, items: Items) -> int:
    """返回容量 capacity 内可取得的最大价值（每种物品可取任意多件）。"""
    dp = [0] * (capacity + 1)

    for weight, value in items:
        # 容量正序枚举：dp[c - weight] 可能已经取过当前物品，这正是「可重复取」。
        for c in range(weight, capacity + 1):
            cand = dp[c - weight] + value
            if cand > dp[c]:
                dp[c] = cand

    return dp[capacity]
