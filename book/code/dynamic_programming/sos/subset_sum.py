# SOS DP（子集和）：把 f 就地变换为 f'[mask] = 原 f 中所有 sub ⊆ mask 的 f[sub] 之和。
# 要求 len(f) >= 1 << n（f 以 0 下标的 mask 索引）；n = 0 时只有 f[0] 一项，变换后不变。
# C++ 用 long long 累加 2^n 项可能溢出；Python int 任意精度，该坑不存在。


def subset_sum(n: int, f: list[int]) -> None:
    """就地累加：对每个 bit 令 f[mask] += f[mask ^ (1 << bit)]（mask 含该位时）。"""
    for bit in range(n):
        for mask in range(1 << n):
            if mask & (1 << bit):
                # 此时 mask ^ (1 << bit) 就是「去掉 bit 位」的子集，它已被本轮之前的
                # bit 处理过、且不含 bit 位，故不会重复累加同一个子集。
                f[mask] += f[mask ^ (1 << bit)]
