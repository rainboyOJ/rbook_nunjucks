# 差分约束：把 x_v - x_u <= w 统一成边 u -> v 权 w，跑单源最短路判可行性、求差值上界。
# 点编号 1..n（dis[0] 不用）。add_leq / add_geq / add_eq 只负责统一不等号方向，不检查输入。
# 边权可正可负，所以用 SPFA；出现负环表示约束矛盾。
# Python 的 int 是任意精度，不必像 C++ 那样在相加前判溢出。

from collections import deque

INF = 1 << 62  # 哨兵，比任何合法差值都大

type WeightedAdj = list[list[tuple[int, int]]]  # graph[u]：若干 (终点 v, 边权 w)


def add_leq(graph: WeightedAdj, u: int, v: int, w: int) -> None:
    """约束 x_v - x_u <= w，也就是 x_v <= x_u + w。"""
    graph[u].append((v, w))


def add_geq(graph: WeightedAdj, u: int, v: int, w: int) -> None:
    """约束 x_u - x_v >= w，也就是 x_v <= x_u - w。"""
    graph[u].append((v, -w))


def add_eq(graph: WeightedAdj, u: int, v: int) -> None:
    """约束 x_u == x_v，拆成 x_u - x_v <= 0 与 x_v - x_u <= 0。"""
    add_leq(graph, u, v, 0)
    add_leq(graph, v, u, 0)


def feasible(graph: WeightedAdj) -> list[int] | None:
    """全局可行性：返回一组可行解 dis（dis[0] 不用）；约束自相矛盾时返回 None。

    所有点距离取 0 并全部入队，等价于加一个到每个点都是 0 权边的超级源点：
    初值只是假定值，整体平移不影响合法性，所以取 0 最省事。
    用路径边数判负环：更短的路走了至少 n 条边，路径上必然绕出了负环。
    """
    n = len(graph) - 1
    dis = [0] * (n + 1)
    path_len = [0] * (n + 1)  # 当前 dis 对应的路径边数
    in_queue = [True] * (n + 1)
    q = deque(range(1, n + 1))

    while q:
        u = q.popleft()
        in_queue[u] = False
        for v, w in graph[u]:
            shorter = dis[u] + w
            if shorter >= dis[v]:
                continue  # 这条边给不出更紧的上界
            dis[v] = shorter
            path_len[v] = path_len[u] + 1
            if path_len[v] >= n:
                return None
            if not in_queue[v]:
                q.append(v)
                in_queue[v] = True

    return dis


def shortest(graph: WeightedAdj, source: int) -> list[int] | None:
    """单源最短路：返回 dis（dis[source] = 0，不可达为 INF）；有可达负环时返回 None。

    与 feasible() 分开，是因为很多题只要求某个点在它自己的连通块里无矛盾：
    这里只从 source 出发，别的连通块里的负环发现不了。
    """
    n = len(graph) - 1
    dis = [INF] * (n + 1)
    path_len = [0] * (n + 1)
    in_queue = [False] * (n + 1)
    dis[source] = 0
    in_queue[source] = True
    q = deque([source])

    while q:
        u = q.popleft()
        in_queue[u] = False
        for v, w in graph[u]:
            shorter = dis[u] + w
            if shorter >= dis[v]:
                continue
            dis[v] = shorter
            path_len[v] = path_len[u] + 1
            if path_len[v] >= n:
                return None
            if not in_queue[v]:
                q.append(v)
                in_queue[v] = True

    return dis
