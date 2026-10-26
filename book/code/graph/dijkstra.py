# Dijkstra 非负权单源最短路（堆优化），点编号 1..n（dist[0] 不用）。
# dijkstra 返回 dist：INF 表示不可达；dist 是局部变量，换源点可以重复调用。
# 边权必须非负；出现负权边时本文的距离不再可信，应改用 Bellman-Ford 或 SPFA。
# Python 的 int 是任意精度，不必像 C++ 那样在相加前判溢出。

import heapq

INF = 1 << 62  # 哨兵，比任何合法最短距离都大

type WeightedAdj = list[list[tuple[int, int]]]  # graph[u]：若干 (终点 v, 边权 w)


def build_graph(
    n: int, edges: list[tuple[int, int, int]], directed: bool = True
) -> WeightedAdj:
    """把 (u, v, w) 边表转成 1..n 的邻接表；无向图把每条边存成两个方向。"""
    graph: WeightedAdj = [[] for _ in range(n + 1)]
    for u, v, w in edges:
        graph[u].append((v, w))
        if not directed:
            graph[v].append((u, w))
    return graph


def dijkstra(graph: WeightedAdj, source: int) -> list[int]:
    """返回 dist[1..n]；同一张图换源点可重复调用。"""
    n = len(graph) - 1
    dist = [INF] * (n + 1)
    dist[source] = 0
    heap: list[tuple[int, int]] = [(0, source)]  # (距离, 点编号)，距离在前才能按它比大小

    while heap:
        du, u = heapq.heappop(heap)
        if du != dist[u]:
            continue  # 过期记录：u 已有更短距离，不要再扫它的出边
        for v, w in graph[u]:
            nd = du + w
            if nd < dist[v]:  # 只接受严格改善，零权环上相等距离不再入堆
                dist[v] = nd
                heapq.heappush(heap, (nd, v))

    return dist
