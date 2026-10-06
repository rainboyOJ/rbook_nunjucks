# 前缀和：s[i] = a[0] + ... + a[i-1]，s[0] = 0，a 用 0 下标存储。
# 查询用 1 下标闭区间 [l, r]，s[r] - s[l-1]。
# C++ 用 long long 防止前缀和溢出 int；Python int 任意精度，该坑不存在。

type Sums = list[int]


class PrefixSum:
    """s 的长度为 n + 1，s[0] 恒为 0，充当查询 l=1 时的左哨兵。"""

    s: Sums

    def __init__(self, a: Sums | None = None) -> None:
        # C++ 的 explicit 构造与默认构造对应 Python 的可选参数。
        if a is None:
            a = []
        self.init(a)

    def init(self, a: Sums) -> None:
        self.s = [0] * (len(a) + 1)
        for i in range(1, len(a) + 1):
            self.s[i] = self.s[i - 1] + a[i - 1]

    def query(self, l: int, r: int) -> int:
        # 调用方需保证 1 <= l <= r <= n；l=0 会取到 s[-1]，越界语义与 C++ 不同。
        return self.s[r] - self.s[l - 1]
