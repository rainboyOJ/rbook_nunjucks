# 多重背包（二进制拆分）：第 i 种物品有 amount 件，每件重 weight、价值 value。
# 把 amount 拆成 1,2,4,...,剩余 这些块，每块当作一件 01 物品，再做 01 背包。
# C++ 用 int 存 pack_weight / pack_value，可能溢出 int；Python int 任意精度。

type Item = tuple[int, int, int]  # (weight, value, amount)
type Items = list[Item]


def multiple_binary(capacity: int, items: Items) -> int:
    """返回容量 capacity 内可取得的最大价值（每种物品有件数上限）。"""
    dp = [0] * (capacity + 1)

    for weight, value, amount in items:
        block = 1  # 块大小 1, 2, 4, 8 ... 二进制拆分
        while amount > 0:
            cnt = block if block < amount else amount  # 对应 C++ 的 min(block, amount)
            amount -= cnt

            pack_weight = cnt * weight
            pack_value = cnt * value

            # 01 背包必须倒序枚举容量，保证这一块只被用一次。
            for c in range(capacity, pack_weight - 1, -1):
                cand = dp[c - pack_weight] + pack_value
                if cand > dp[c]:
                    dp[c] = cand

            block <<= 1  # 翻倍：1→2→4→…（C++ 的 int 会溢出，Python 不会）

    return dp[capacity]
