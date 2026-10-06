# 单点 chmax、前缀最大值。
# 更新只能写成 a[pos] = max(a[pos], value)，不能任意赋值：树状数组上的块
# 只维护"块内最大值"，任意赋值无法下推到旧块，会破坏不变量。
# identity 取 -inf 对应 C++ 的 numeric_limits<T>::lowest()。
# 下标必须从 1 开始：lowbit(0) = 0，会让修改循环原地打转。
# 调用示例：fm = FenwickPrefixMax(5); fm.chmax(3, 7); fm.prefix_max(5)


class FenwickPrefixMax:
    """树状数组维护前缀最大值：tree[i] 维护 a[i - lowbit(i) + 1 .. i] 的最大值。"""

    n: int
    identity: int
    tree: list[int]

    def __init__(self, size: int = 0) -> None:
        self.init(size)

    def init(self, size: int) -> None:
        """重置为长度 size 的空数组，所有位置先放 identity（即 -inf）。"""
        self.n = size
        # identity 必须满足“任何真实值都比它大”，取 -2**63 对应
        # C++ 的 numeric_limits<long long>::lowest()，两边对拍时结果一致。
        self.identity = -(2**63)
        self.tree = [self.identity] * (size + 1)

    @staticmethod
    def lowbit(x: int) -> int:
        """块长：x 二进制最低位那个 1 代表的值，也就是 x & -x。"""
        return x & -x

    def chmax(self, pos: int, value: int) -> None:
        """单点 chmax：a[pos] = max(a[pos], value)。"""
        i = pos
        while i <= self.n:
            if value > self.tree[i]:
                self.tree[i] = value
            i += self.lowbit(i)

    def prefix_max(self, pos: int) -> int:
        """前缀最大值：max(a[1], ..., a[pos])，每次并入一个以 i 结尾的完整块。

        pos = 0 时返回 identity（空数组没有最大值，由调用方自行约定语义）。
        """
        answer = self.identity
        i = pos
        while i > 0:
            if self.tree[i] > answer:
                answer = self.tree[i]
            i -= self.lowbit(i)
        return answer
