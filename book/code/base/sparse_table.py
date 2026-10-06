# ST 表：预处理所有长度为 2^k 的区间最值，O(1) 查询静态 RMQ。
# a 必须是 1 下标（a[0] 不用，仅占位），build 前布置好；查询要求 1 <= l <= r <= n。
# st[i][k] = 从 i 开始、长度为 2^k 的区间最大值；查询用两个可重叠的 2^k 区间覆盖
# [l, r]（重叠不影响 max/min/gcd 等幂等运算）。
# C++ 用 int 存值，1e9 量级两次 max 不会溢出；Python int 任意精度，该坑不存在。

type Table = list[list[int]]  # st[i][k]：st 的第 i 行是长度 max_log 的列表


class SparseTable:
    """n 为数据长度；lg[x] = floor(log2(x))；st 见文件头注释。"""

    def __init__(self) -> None:
        self.n = 0
        self.lg: list[int] = []
        self.st: Table = []

    def build(self, a: list[int]) -> None:
        self.n = len(a) - 1  # a 使用 1-indexed

        # 预处理 log2 表，避免每次查询重复计算；lg[i/2] + 1 即 floor(log2(i))。
        self.lg = [0] * (self.n + 1)
        for i in range(2, self.n + 1):
            self.lg[i] = self.lg[i // 2] + 1

        max_log = self.lg[self.n] + 1
        self.st = [[0] * max_log for _ in range(self.n + 1)]

        # k=0：长度为 1 的区间就是自己。
        for i in range(1, self.n + 1):
            self.st[i][0] = a[i]

        # k>0：长度为 2^k 的区间 = 左 2^{k-1} + 右 2^{k-1}。
        for k in range(1, max_log):
            length = 1 << k      # 当前区间长度 2^k
            half = length >> 1   # 半长 2^{k-1}
            for i in range(1, self.n - length + 2):  # 保证 i + length - 1 <= n
                self.st[i][k] = max(self.st[i][k - 1], self.st[i + half][k - 1])

    def query(self, l: int, r: int) -> int:
        # k = floor(log2(r - l + 1))；两个 2^k 区间从 l 和 r-(1<<k)+1 出发重叠覆盖 [l, r]。
        k = self.lg[r - l + 1]
        return max(self.st[l][k], self.st[r - (1 << k) + 1][k])
