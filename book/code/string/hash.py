# 字符串哈希：1-based 子串 s[l..r] 的哈希值，O(1) 查询。
# C++ 用 unsigned long long 自然溢出（等价于模 2^64），这里用 MASK 显式模拟回绕，
# 保证与 C++ 数值逐位一致；Python int 本身任意精度，不会溢出。
# get(l, r) 是 1-based 闭区间，l > r（空区间）返回 0，与 C++ 一致。

type Seq = list[int]  # prefix / power：长度 n + 1 的哈希序列，下标 0 为哨兵

MASK: int = (1 << 64) - 1  # 低 64 位全 1；& MASK 即截断到 64 位，等价无符号回绕


class StringHash:
    """prefix[i] = s[1..i] 的哈希，power[i] = BASE^i。"""

    BASE: int = 131  # 进制；C++ 的 static constexpr，Python 放类属性

    prefix: Seq
    power: Seq

    def __init__(self, s: str) -> None:
        n = len(s)
        self.prefix = [0] * (n + 1)
        self.power = [0] * (n + 1)
        self.power[0] = 1  # BASE^0 = 1

        for i in range(1, n + 1):
            # C++ 取 (unsigned char)s[i-1]，即字节值；Python 用 ord 码点，
            # 对 ASCII 两者相同，非 ASCII 串请先 encode 成 bytes 再逐字节传入。
            self.power[i] = (self.power[i - 1] * self.BASE) & MASK
            self.prefix[i] = (self.prefix[i - 1] * self.BASE + ord(s[i - 1])) & MASK

    def get(self, l: int, r: int) -> int:
        """子串 s[l..r] 的哈希（1-based 闭区间）；空区间返回 0。"""
        if l > r:
            return 0
        # 把前缀 s[1..l-1] 整体左移 r-l+1 位后减掉，减法同样按 64 位回绕。
        return (self.prefix[r] - self.prefix[l - 1] * self.power[r - l + 1]) & MASK
