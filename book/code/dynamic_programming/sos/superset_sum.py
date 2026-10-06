# SOS DP（超集和）：把 f 就地变换为 f'[mask] = 原 f 中所有 sup ⊇ mask 的 f[sup] 之和。
# 要求 len(f) >= 1 << n（f 以 0 下标的 mask 索引）；n = 0 时只有 f[0] 一项，变换后不变。
# C++ 用 long long 累加 2^n 项可能溢出；Python int 任意精度，该坑不存在。


def superset_sum(n: int, f: list[int]) -> None:
    """就地累加：对每个 bit 令 f[mask] += f[mask ^ (1 << bit)]（mask 不含该位时）。"""
    for bit in range(n):
        for mask in range(1 << n):
            if (mask & (1 << bit)) == 0:
                # mask ^ (1 << bit) 就是「加上 bit 位」的超集，它与 mask 的其余位相同，
                # 且不含更低位的新增位，故每个超集恰好贡献一次。
                f[mask] += f[mask ^ (1 << bit)]
