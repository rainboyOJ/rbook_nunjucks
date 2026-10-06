# 差分数组：diff[i] = a[i] - a[i-1]，对 1 下标闭区间 [l, r] 整体加 v 只改两端。
# restore 前缀和还原后，diff[r+1] 的减法会把加法的影响截断在 r 处。
# C++ 用 long long 防止区间累加溢出；Python int 任意精度，该坑不存在。

type Seq = list[int]


class DifferenceArray:
    """一维差分。a 用 0 下标，diff 和 restore 出来的原数组用 1 下标。"""

    n: int
    diff: Seq

    def __init__(self, a: Seq | None = None) -> None:
        if a is None:
            a = []
        self.init(a)

    def init(self, a: Seq) -> None:
        """a 使用 0 下标存储；diff 使用 1 下标，方便处理区间 [l, r]。"""
        self.n = len(a)
        self.diff = [0] * (self.n + 2)
        for i in range(1, self.n + 1):
            self.diff[i] = a[i - 1] - (0 if i == 1 else a[i - 2])

    def add(self, l: int, r: int, v: int) -> None:
        """对原数组的 1 下标闭区间 [l, r] 全部加 v。"""
        self.diff[l] += v
        self.diff[r + 1] -= v

    def restore(self) -> Seq:
        """将差分数组还原成 1 下标原数组。"""
        a: Seq = [0] * (self.n + 1)
        for i in range(1, self.n + 1):
            a[i] = a[i - 1] + self.diff[i]
        return a
