# 枚举 mask 的所有非空子掩码：sub 的 1 只能出现在 mask 为 1 的位置。
# 空集 0 不在返回值里，需要枚举空集时在调用处单独处理。
# C++ 用 int 存掩码；Python int 任意精度，理论上掩码位数超过 30 也不会溢出。

type Masks = list[int]


def non_empty_submasks(mask: int) -> Masks:
    # sub = (sub - 1) & mask：把 sub 的最低位 1 去掉后，
    # 再用 & mask 补回该位以下 mask 中存在的位，保证按字典序从大到小枚举。
    res: Masks = []
    sub = mask
    while sub:
        res.append(sub)
        sub = (sub - 1) & mask
    return res


def print_bits(x: int, n: int) -> str:
    # 与 C++ 一致：只输出低 n 位；调用方自行决定拼接方式（C++ 版直接写 stdout）。
    return "".join("1" if (x >> i) & 1 else "0" for i in range(n - 1, -1, -1))
