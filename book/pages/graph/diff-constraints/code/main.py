#!/usr/bin/env python3
# 差分约束求 x_s - x_t 的最大值。
# 输入：第一行 n m s t；接下来 m 行 a b c，表示约束 x_a - x_b <= c（点编号 1..n）。
# 输出：x_s - x_t 的最大值；约束矛盾输出 NO；差值没有上界输出 UNBOUNDED。

import sys
from collections import deque

INF = 1 << 62  # 哨兵，比任何合法差值都大

type WeightedAdj = list[list[tuple[int, int]]]  # graph[u]：若干 (终点 v, 边权 w)


def feasible(graph: WeightedAdj) -> bool:
    """全局可行性：所有点距离取 0 并全部入队，等价于加一个 0 权超级源点。

    用路径边数判环：更短的路走了至少 n 条边，路径上必然绕出了负环。
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
                return False
            if not in_queue[v]:
                q.append(v)
                in_queue[v] = True

    return True


def shortest(graph: WeightedAdj, source: int) -> list[int]:
    """单源最短路：返回 dis（dis[source] = 0，不可达为 INF）。

    调用前必须已用 feasible() 排除全图负环，所以这里不再重复判环。
    """
    n = len(graph) - 1
    dis = [INF] * (n + 1)
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
            if not in_queue[v]:
                q.append(v)
                in_queue[v] = True

    return dis


def solve() -> None:
    data = iter(map(int, sys.stdin.buffer.read().split()))
    n, m = next(data), next(data)
    s, t = next(data), next(data)

    graph: WeightedAdj = [[] for _ in range(n + 1)]
    for _ in range(m):
        a, b, c = next(data), next(data), next(data)
        graph[b].append((a, c))  # x_a - x_b <= c 就是 x_a <= x_b + c

    if not feasible(graph):
        print('NO')  # 约束自相矛盾
        return

    dis = shortest(graph, t)  # 已确认无负环，t 到 s 的最短路就是 x_s - x_t 的上界
    print('UNBOUNDED' if dis[s] == INF else dis[s])


if __name__ == "__main__":
    solve()
