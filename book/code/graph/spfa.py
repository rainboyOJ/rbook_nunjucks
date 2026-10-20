# SPFA 单源最短路，可判从 source 可达的负环；点编号 1..n（dis[0] 不用）。
# 返回 dis 表示成功，dis 用 INF 表示不可达；返回 None 表示存在从 source 可达的负环。
# cnt 记录当前 dis 对应路径的边数，不是更新次数；达到 n 说明绕出了负环。
# 边权可正可负，所以不能用 Dijkstra；Python int 是任意精度，不必像 C++ 那样防溢出。

from collections import deque

INF = 1 << 62  # 哨兵，比任何合法最短距离都大

type WeightedAdj = list[list[tuple[int, int]]]  # graph[u]：若干 (终点 v, 边权 w)


def add_edge(graph: WeightedAdj, u: int, v: int, w: int) -> None:
    """加一条有向边 u -> v，权 w。点编号从 1 开始，graph[0] 不用。"""
    graph[u].append((v, w))


def spfa(graph: WeightedAdj, source: int) -> list[int] | None:
    """返回 dis（dis[source] = 0，不可达为 INF）；有可达负环时返回 None。

    dis 与 cnt 都是局部变量，所以同一张图换源点可以反复调用，不会残留上一次的状态。
    """
    n = len(graph) - 1
    dis = [INF] * (n + 1)
    path_len = [0] * (n + 1)  # 当前 dis 对应的路径边数
    in_queue = [False] * (n + 1)
    dis[source] = 0
    in_queue[source] = True
    q = deque([source])

    while q:
        u = q.popleft()
        in_queue[u] = False
        for v, w in graph[u]:
            shorter = dis[u] + w  # 入队的点一定可达，不会拿 INF 参加加法
            if shorter >= dis[v]:
                continue
            # 即使 v 已经在队列里，也必须先更新 dis 和 path_len，只省掉重复入队。
            dis[v] = shorter
            path_len[v] = path_len[u] + 1
            if path_len[v] >= n:
                return None
            if not in_queue[v]:
                q.append(v)
                in_queue[v] = True

    return dis
