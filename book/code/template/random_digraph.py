# 随机有向图：在 1..n 上对每对 u != v 以概率 p 连有向边 u -> v（不含自环）。
# 随机源是模块级 _rng（对应 C++ 全局 rng）；复现前先 _rng.seed(固定种子)。只返回边表。
# 调用示例：_rng.seed(1); edges = random_digraph(5, 0.30)。

import random

type Edges = list[tuple[int, int]]  # 边表：每个元素是一条边的 (u, v)

_rng = random.Random()


def hit(probability: float) -> bool:
    """以 probability 的概率返回 True；random() 取 [0, 1)，与 C++ uniform_real 的 <= 等价。"""
    return _rng.random() <= probability


def random_digraph(n: int = 5, p: float = 0.30) -> Edges:
    """随机有向图：对每个有序点对 u != v 独立决定是否连边，可能同时有 u->v 与 v->u。"""
    edges: Edges = []
    for u in range(1, n + 1):
        for v in range(1, n + 1):
            if u != v and hit(p):
                edges.append((u, v))
    return edges
