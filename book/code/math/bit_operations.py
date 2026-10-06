# 位运算工具集，对应 C++ 的 unsigned long long（64 位无符号）语义。
# 约定：x 为非负整数且 x < 2^64，位下标 k 满足 0 <= k < 64；超出这个范围的结果
# 不会像 C++ 那样对 2^64 回绕（Python int 是任意精度，不做取模裁剪）。

type BitList = list[int]  # non_empty_subsets 返回的子集列表


def has_bit(x: int, k: int) -> bool:
    # 右移 k 位后最低位就是原来的第 k 位。
    return ((x >> k) & 1) == 1


def set_bit(x: int, k: int) -> int:
    # 1 << k 是只有第 k 位为 1 的掩码。
    return x | (1 << k)


def clear_bit(x: int, k: int) -> int:
    # Python 的 ~(1 << k) 是无限位补码，但对非负 x 做 & 的结果与 C++ 的 64 位掩码一致。
    return x & ~(1 << k)


def flip_bit(x: int, k: int) -> int:
    return x ^ (1 << k)


def lowbit(x: int) -> int:
    # -x 等于按位取反再加一，与 x 相与后只剩最低位的那个 1。
    return x & -x


def clear_lowbit(x: int) -> int:
    # x - 1 把最低位的 1 变成 0、更低位全变成 1，再与 x 相与正好抹掉这个 1。
    return x & (x - 1)


def count_bits(x: int) -> int:
    # 对应 __builtin_popcountll，统计二进制表示里 1 的个数。
    return bin(x).count("1")


def highest_bit_pos(x: int) -> int:
    # 最高位 1 的下标；0 没有最高位，返回 -1（与 C++ 一致）。
    if x == 0:
        return -1
    return x.bit_length() - 1


def keep_highbit(x: int) -> int:
    if x == 0:
        return 0
    return 1 << highest_bit_pos(x)


def clear_highbit(x: int) -> int:
    # 异或掉最高位的那一个 1。
    return x ^ keep_highbit(x)


def is_power_of_two(x: int) -> bool:
    # 2 的幂只有一个 1，清掉最低位的 1 后应变成 0；0 不是 2 的幂。
    return x > 0 and (x & (x - 1)) == 0


def non_empty_subsets(mask: int) -> BitList:
    res: BitList = []
    # (sub - 1) & mask 得到按数值降序的下一个子集，能不重不漏地枚举完所有非空子集。
    sub = mask
    while sub:
        res.append(sub)
        sub = (sub - 1) & mask
    return res
