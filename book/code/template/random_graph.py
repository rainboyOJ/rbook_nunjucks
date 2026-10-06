# 随机无向简单图：在 1..n 上对每对 u < v 以概率 p 连一条边（每对点至多一条）。
# 随机源是模块级 _rng（对应 C++ 全局 rng）；复现前先 _rng.seed(固定种子)。只返回边表。
# 调用示例：_rng.seed(1); edges = random_graph(6, 0.35)。

import random

type Edges = list[tuple[int, int]]  # 边表：每个元素是一条边的 (u, v)

_rng = random.Random()


def hit(probability: float) -> bool:
    """以 probability 的概率返回 True；random() 取 [0, 1)，与 C++ uniform_real 的 <= 等价。"""
    return _rng.random() <= probability


def random_graph(n: int = 6, p: float = 0.35) -> Edges:
    """随机无向图：只生成 u < v 的边，边数随机，期望 p * n * (n - 1) / 2。"""
    edges: Edges = []
    for u in range(1, n + 1):
        for v in range(u + 1, n + 1):
            if hit(p):
                edges.append((u, v))
    return edges
