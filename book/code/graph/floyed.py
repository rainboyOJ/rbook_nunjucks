# Floyd-Warshall 全源最短路：dist[i][j] 为 i 到 j 的最短路，INF 表示不可达。
# 点编号 1..n，返回 (n+1)x(n+1) 矩阵（dist[i][i] = 0，下标 0 不用）；边以 (u, v, w) 传入。
# C++ 的 long long 与 INF = 1LL << 60 在 Python 里是任意精度 int，不会溢出。
# 平行边只保留最小权值；不处理负环（有负环时结果无意义）。

INF = 1 << 60  # 哨兵，约 1.15e18，比任何合法路径都大

type Grid = list[list[int]]


def floyd(n: int, edges: list[tuple[int, int, int]]) -> Grid:
    """返回 n+1 阶距离矩阵；dist[0][*] 与 dist[*][0] 恒为 INF，不使用。"""
    dist: Grid = [[INF] * (n + 1) for _ in range(n + 1)]
    for i in range(1, n + 1):
        dist[i][i] = 0  # 自己到自己距离 0

    for u, v, w in edges:
        if w < dist[u][v]:  # 平行边保留最小权值，对应 C++ 的 min(dist[u][v], w)
            dist[u][v] = w

    for k in range(1, n + 1):
        for i in range(1, n + 1):
            if dist[i][k] == INF:
                continue  # i 到中转点 k 不可达，松弛无意义
            for j in range(1, n + 1):
                if dist[k][j] == INF:
                    continue
                nd = dist[i][k] + dist[k][j]
                if nd < dist[i][j]:
                    dist[i][j] = nd
    return dist
