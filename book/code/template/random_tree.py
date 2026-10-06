# 生成 n 个点的随机树（1 下标）：点 i 的父节点从 [1, i-1] 均匀取，边数恒为 n-1。
# 随机源是模块级 _rng（对应 C++ 全局 __rnd）；复现前先 _rng.seed(固定种子)。
# 只返回边表，不打印；调用示例：_rng.seed(1); edges = random_tree(6)。

import random

type Edges = list[tuple[int, int]]  # 边表：每个元素是一条边的 (u, v)

_rng = random.Random()


def rnd(l: int, r: int) -> int:
    """返回 [l, r] 内的随机整数；randrange 无 C++ 取模偏置，边界含两端。"""
    return _rng.randrange(l, r + 1)


class MyShuffle:
    """构造时把 [1, n] 洗成随机排列，get() 依次吐出，共 n 个不重复值。"""

    def __init__(self, n: int) -> None:
        tail = list(range(1, n + 1))
        _rng.shuffle(tail)
        # a[0] 是占位，与 C++ 的 1 下标数组对齐；get() 从 a[1] 开始取。
        self.a = [0] + tail
        self.idx = 0

    def get(self) -> int:
        self.idx += 1
        return self.a[self.idx]


def random_tree(n: int = 6) -> Edges:
    """n 个点的随机树，边为 (父, 子) 且父编号严格小于子编号。n < 1 时返回空表。"""
    edges: Edges = []
    for i in range(2, n + 1):
        p = rnd(1, i - 1)
        edges.append((p, i))
    return edges
