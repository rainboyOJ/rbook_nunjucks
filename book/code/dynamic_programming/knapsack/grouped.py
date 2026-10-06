# 分组背包：每组至多选一件物品，求容量 capacity 内的最大价值。
# groups 是若干组，每组为 (weight, value) 列表；容量 0 下标，dp[c] 含义同 01 背包。
# 每组必须从「上一组结束时的 dp」转移，故先复制 previous，避免同组内串用导致多选。

type Item = tuple[int, int]  # (weight, value)
type Groups = list[list[Item]]


def grouped(capacity: int, groups: Groups) -> int:
    """返回容量 capacity 内可取得的最大价值（每组至多选一件）。"""
    dp = [0] * (capacity + 1)

    for group in groups:
        previous = dp[:]  # 快照：本组内所有转移都只允许用它
        for c in range(capacity + 1):
            best = previous[c]  # 本组一件都不选
            for weight, value in group:
                if c < weight:
                    continue  # 容量不足，这件装不下
                cand = previous[c - weight] + value
                if cand > best:
                    best = cand
            dp[c] = best

    return dp[capacity]
