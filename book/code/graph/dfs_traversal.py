# 无向图 DFS 遍历：order 记录访问顺序，visited[u] = 1 表示已访问；点编号 1..n。
# DFS 顺序取决于 adj[u] 的存储顺序，源 C++ main 会先对每个邻接表排序，想复刻需自行 sort。
# 只从 dfs 的起点所在连通块出发，不主动遍历全图。
# dfs 递归深度最坏 n（1e5+ 长链），使用前需 sys.setrecursionlimit(1 << 20)。

type Adj = list[list[int]]


class Graph:
    """visited 与 order 由 dfs 增量维护；重复 dfs 同一批点不会重复记录。"""

    n: int
    adj: Adj
    visited: list[int]
    order: list[int]

    def __init__(self, n: int) -> None:
        self.n = n
        self.adj = [[] for _ in range(n + 1)]
        self.visited = [0] * (n + 1)
        self.order = []

    def add_edge(self, u: int, v: int) -> None:
        self.adj[u].append(v)
        self.adj[v].append(u)

    def dfs(self, u: int) -> None:
        self.visited[u] = 1
        self.order.append(u)  # 入栈（第一次访问）时就记录，得到先序遍历
        for v in self.adj[u]:
            if self.visited[v]:
                continue
            self.dfs(v)
