# 单点加、前缀和、区间和。
# 下标必须从 1 开始：lowbit(0) = 0，会让修改循环原地打转。
# Python 的 int 是任意精度，不必像 C++ 那样担心 long long 溢出。


class Fenwick:
    """树状数组：tree[i] 维护 a[i - lowbit(i) + 1 .. i] 的元素和。"""

    n: int
    tree: list[int]

    def __init__(self, size: int = 0) -> None:
        self.init(size)

    def init(self, size: int) -> None:
        """重置为长度 size 的空数组，方便同一个对象换一组数据复用。"""
        self.n = size
        self.tree = [0] * (size + 1)

    @staticmethod
    def lowbit(x: int) -> int:
        """块长：x 二进制最低位那个 1 代表的值，也就是 x & -x。"""
        return x & -x

    def add(self, pos: int, value: int) -> None:
        """单点加：a[pos] += value。所有覆盖 pos 的块都要加上 value。"""
        i = pos
        while i <= self.n:
            self.tree[i] += value
            i += self.lowbit(i)

    def prefix_sum(self, pos: int) -> int:
        """前缀和：a[1] + a[2] + ... + a[pos]，每次取走一个以 i 结尾的完整块。"""
        answer = 0
        i = pos
        while i > 0:
            answer += self.tree[i]
            i -= self.lowbit(i)
        return answer

    def range_sum(self, left: int, right: int) -> int:
        """区间和：a[left] + ... + a[right]，用两个前缀和相减得到。"""
        return self.prefix_sum(right) - self.prefix_sum(left - 1)
