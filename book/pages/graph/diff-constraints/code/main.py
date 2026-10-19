#!/usr/bin/env python3
# 差分约束求 x_s - x_t 的最大值。
# 输入：第一行 n m s t；接下来 m 行 a b c，表示约束 x_a - x_b <= c（点编号 1..n）。
# 输出：x_s - x_t 的最大值；约束矛盾输出 NO；差值没有上界输出 UNBOUNDED。

import sys
from collections import deque

INF = 1 << 62

type WeightedAdj = list[list[tuple[int, int]]]


def feasible(graph: WeightedAdj, n: int) -> tuple[bool, list[int]]:
    """全局可行性：所有点距离取 0 并全部入队，等价于加一个 0 权超级源点。"""
    dis = [0] * (n + 1)
    cnt = [0] * (n + 1)
    in_queue = [True] * (n + 1)
    q = deque(range(1, n + 1))

    while q:
        u = q.popleft()
        in_queue[u] = False
        for v, w in graph[u]:
            if dis[u] + w >= dis[v]:
                continue
            dis[v] = dis[u] + w
            cnt[v] = cnt[u] + 1
            if cnt[v] >= n:
                return False, dis
            if not in_queue[v]:
                q.append(v)
                in_queue[v] = True

    return True, dis


def shortest(graph: WeightedAdj, n: int, source: int) -> tuple[bool, list[int]]:
    """单源最短路；返回 (是否无负环, dis)。"""
    dis = [INF] * (n + 1)
    cnt = [0] * (n + 1)
    in_queue = [False] * (n + 1)
    dis[source] = 0
    in_queue[source] = True
    q = deque([source])

    while q:
        u = q.popleft()
        in_queue[u] = False
        for v, w in graph[u]:
            candidate = dis[u] + w
            if candidate >= dis[v]:
                continue
            dis[v] = candidate
            cnt[v] = cnt[u] + 1
            if cnt[v] >= n:
                return False, dis
            if not in_queue[v]:
                q.append(v)
                in_queue[v] = True

    return True, dis


def solve() -> None:
    data = iter(map(int, sys.stdin.buffer.read().split()))
    n, m = next(data), next(data)
    s, t = next(data), next(data)

    graph: WeightedAdj = [[] for _ in range(n + 1)]
    for _ in range(m):
        a, b, c = next(data), next(data), next(data)
        graph[b].append((a, c))  # x_a - x_b <= c 就是 x_a <= x_b + c

    ok, _ = feasible(graph, n)
    if not ok:
        print('NO')  # 约束自相矛盾
        return

    _, dis = shortest(graph, n, t)  # 已确认无负环，t 到 s 的最短路就是上界
    print('UNBOUNDED' if dis[s] == INF else dis[s])


if __name__ == "__main__":
    solve()
