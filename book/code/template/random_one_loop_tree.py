# 生成 n 个点的「基环树」：先按 random_tree 的方式造 n-1 条树边，再补一条额外边。
# 要求 n >= 3：n = 2 时额外边的两个端点只能与唯一树边重合，C++ 原版同样会死循环。
# 随机源是模块级 _rng（对应 C++ 全局 __rnd）；复现前先 _rng.seed(固定种子)。只返回边表。

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


def random_one_loop_tree(n: int = 6) -> Edges:
    """n 个点、n 条边的基环树（恰好一个环）；父边满足父编号小于子编号。"""
    edges: Edges = []
    parent = [0] * (n + 1)
    for i in range(2, n + 1):
        p = rnd(1, i - 1)
        edges.append((p, i))
        parent[i] = p

    while True:
        p = rnd(1, n - 1)
        # parent[n] 已经连向 n，再连同一条边就只是重边，环不成立，重摇。
        if parent[n] == p:
            continue
        edges.append((p, n))
        break
    return edges
