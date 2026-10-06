# 区间加（维护差分数组）、单点查询。
# 下标必须从 1 开始：lowbit(0) = 0，会让修改循环原地打转。
# Python 的 int 是任意精度，不必像 C++ 那样担心 long long 溢出。
# 调用示例：fa = RangeAddPointQueryFenwick(5); fa.range_add(1, 3, 2); fa.point_query(3)


class RangeAddPointQueryFenwick:
    """树状数组上维护差分：tree[i] 维护 diff[i - lowbit(i) + 1 .. i] 的元素和。

    区间加等价于差分数组上两次单点加，单点查询等价于差分前缀和。
    """

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
        """给差分数组的一个位置增加 value：diff[pos] += value。"""
        i = pos
        while i <= self.n:
            self.tree[i] += value
            i += self.lowbit(i)

    def prefix_sum(self, pos: int) -> int:
        """差分前缀和：diff[1] + ... + diff[pos]，恰好等于 a[pos] 的当前值。"""
        answer = 0
        i = pos
        while i > 0:
            answer += self.tree[i]
            i -= self.lowbit(i)
        return answer

    def range_add(self, left: int, right: int, value: int) -> None:
        """区间加：a[left..right] 都加上 value。

        等价于 diff[left] += value 且 diff[right + 1] -= value；
        right + 1 越过末尾时无需修改（C++ 同样有这个 if 保护）。
        """
        self.add(left, value)
        if right + 1 <= self.n:
            self.add(right + 1, -value)

    def point_query(self, pos: int) -> int:
        """单点查询：返回 a[pos] 的当前值。"""
        return self.prefix_sum(pos)
