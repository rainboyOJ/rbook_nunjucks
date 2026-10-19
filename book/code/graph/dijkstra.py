# Dijkstra 非负权单源最短路（堆优化），点编号 1..n（dist[0] 不用）。
# dijkstra 返回 (dist, pre)：dist 用 INF 表示不可达，pre[source] = -1，其余前驱在松弛成功时记录。
# 边权必须非负；出现负权边时本文的距离不再可信，应改用 Bellman-Ford 或 SPFA。
# Python 的 int 是任意精度，不必像 C++ 那样用 long long 防溢出。

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


def dijkstra(graph: WeightedAdj, source: int) -> tuple[list[int], list[int]]:
    """返回 (dist, pre)；同一张图换源点可重复调用。"""
    n = len(graph) - 1
    dist = [INF] * (n + 1)
    pre = [-1] * (n + 1)
    dist[source] = 0
    heap: list[tuple[int, int]] = [(0, source)]  # (距离, 点编号)，距离在前才能按它比大小

    while heap:
        du, u = heapq.heappop(heap)
        if du != dist[u]:
            continue  # 旧记录：这个点已有更短距离，不要再扫它的出边
        for v, w in graph[u]:
            nd = du + w
            if nd < dist[v]:  # 只接受严格改善，零权环上相等距离不再入堆
                dist[v] = nd
                pre[v] = u  # 前驱指向更早确定的点，不会形成环
                heapq.heappush(heap, (nd, v))

    return dist, pre


def path_to(dist: list[int], pre: list[int], target: int) -> list[int]:
    """返回 source 到 target 的一条最短路径（含两端）；不可达返回空列表。"""
    if dist[target] == INF:
        return []
    path: list[int] = []
    u = target
    while u != -1:
        path.append(u)
        u = pre[u]
    return path[::-1]
