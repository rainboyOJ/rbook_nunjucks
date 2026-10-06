# Bellman-Ford 单源最短路，可判从 source 可达的负环；点编号 1..n（dist[0] 不用）。
# 返回 (has_negative_cycle, dist)：dist 用 INF 表示不可达，边权可正可负。
# C++ 的 long long 与 INF = 1LL << 60 在 Python 里是任意精度 int，不会溢出。
# 注意：负环判定只看从 source 可达的边，与「全图是否有负环」不是一回事。

from typing import NamedTuple

INF = 1 << 60  # 哨兵，约 1.15e18，比任何合法路径都大


class Edge(NamedTuple):
    """有向边 from_ -> to，权值 weight；from 是 Python 关键字，故字段写作 from_。"""

    from_: int
    to: int
    weight: int


def bellman_ford(n: int, edges: list[Edge], source: int) -> tuple[bool, list[int]]:
    """返回 (是否存在从 source 可达的负环, dist[1..n])；无负环时 dist 为最短路。"""
    dist = [INF] * (n + 1)
    dist[source] = 0

    # 最短路最多含 n-1 条边，故只需松弛 n-1 轮；某轮没有更新说明已收敛，提前退出。
    for _ in range(n - 1):
        changed = False
        for edge in edges:
            if dist[edge.from_] == INF:
                continue  # 起点还不可达，松弛无意义
            if dist[edge.to] > dist[edge.from_] + edge.weight:
                dist[edge.to] = dist[edge.from_] + edge.weight
                changed = True
        if not changed:
            break

    # 第 n 轮若还能松弛，说明存在从 source 可达的负环（dist 随之失去意义）。
    has_negative_cycle = False
    for edge in edges:
        if dist[edge.from_] == INF:
            continue
        if dist[edge.to] > dist[edge.from_] + edge.weight:
            has_negative_cycle = True
            break

    return has_negative_cycle, dist
