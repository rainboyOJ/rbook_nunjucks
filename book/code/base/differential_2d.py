# 二维差分：对子矩形 [(x1, y1), (x2, y2)] 整体加 v 只改四角；
# restore 按二维前缀和还原，右上/左下两处的减法恰好把影响限制在矩形内。
# 坐标全部从 1 开始，diff 多留一圈（n+2 行 m+2 列）防止 x2+1、y2+1 越界。
# C++ 用 long long 防止累加溢出；Python int 任意精度，该坑不存在。

type Grid = list[list[int]]


class Difference2D:
    n: int
    m: int
    diff: Grid

    def __init__(self, n: int = 0, m: int = 0) -> None:
        self.init(n, m)

    def init(self, n: int, m: int) -> None:
        self.n = n
        self.m = m
        self.diff = [[0] * (m + 2) for _ in range(n + 2)]

    # 对原矩阵的子矩形 [(x1, y1), (x2, y2)] 全部加 v，坐标从 1 开始。
    def add(self, x1: int, y1: int, x2: int, y2: int, v: int) -> None:
        self.diff[x1][y1] += v
        self.diff[x2 + 1][y1] -= v
        self.diff[x1][y2 + 1] -= v
        self.diff[x2 + 1][y2 + 1] += v

    def restore(self) -> Grid:
        a: Grid = [[0] * (self.m + 1) for _ in range(self.n + 1)]
        for i in range(1, self.n + 1):
            for j in range(1, self.m + 1):
                a[i][j] = a[i - 1][j] + a[i][j - 1] - a[i - 1][j - 1] + self.diff[i][j]
        return a
