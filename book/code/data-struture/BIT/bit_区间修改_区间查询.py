# 双树状数组：区间加、区间和。
# bit_diff 维护差分数组 b[i]，bit_weighted 维护 i * b[i]，两者配合把区间和
# 拆成 (pos + 1) * 前缀b - 前缀(i*b)。
# 下标必须从 1 开始：lowbit(0) = 0，会让修改循环原地打转。
# Python 的 int 是任意精度，不必像 C++ 那样担心 long long 溢出。
# 调用示例：fa = RangeFenwick(5); fa.range_add(1, 3, 2); fa.range_sum(2, 5)


class RangeFenwick:
    """区间加区间和的双树状数组。

    推导：a[pos] = sum(b[1..pos])，于是
    sum(a[1..pos]) = (pos + 1) * sum(b[1..pos]) - sum(i * b[i], i = 1..pos)。
    """

    n: int
    bit_diff: list[int]
    bit_weighted: list[int]

    def __init__(self, size: int = 0) -> None:
        self.init(size)

    def init(self, size: int) -> None:
        """重置为长度 size 的空数组，方便同一个对象换一组数据复用。"""
        self.n = size
        self.bit_diff = [0] * (size + 1)
        self.bit_weighted = [0] * (size + 1)

    @staticmethod
    def lowbit(x: int) -> int:
        """块长：x 二进制最低位那个 1 代表的值，也就是 x & -x。"""
        return x & -x

    def add(self, bit: list[int], pos: int, value: int) -> None:
        """在指定的一棵树状数组 bit 上做单点加。"""
        i = pos
        while i <= self.n:
            bit[i] += value
            i += self.lowbit(i)

    def sum(self, bit: list[int], pos: int) -> int:
        """在指定的一棵树状数组 bit 上求前缀和。"""
        answer = 0
        i = pos
        while i > 0:
            answer += bit[i]
            i -= self.lowbit(i)
        return answer

    def range_add(self, left: int, right: int, value: int) -> None:
        """区间加：a[left..right] 都加上 value。

        注意 right + 1 位置的两处修改都不能漏：差分减 value，
        加权数组减 value * (right + 1)，C++ 里同样写的是右端点 + 1。
        """
        self.add(self.bit_diff, left, value)
        self.add(self.bit_diff, right + 1, -value)
        self.add(self.bit_weighted, left, value * left)
        self.add(self.bit_weighted, right + 1, -(value * (right + 1)))

    def prefix_sum(self, pos: int) -> int:
        """前缀和：a[1] + ... + a[pos]。"""
        return (pos + 1) * self.sum(self.bit_diff, pos) - self.sum(
            self.bit_weighted, pos
        )

    def range_sum(self, left: int, right: int) -> int:
        """区间和：a[left] + ... + a[right]，用两个前缀和相减得到。"""
        return self.prefix_sum(right) - self.prefix_sum(left - 1)
