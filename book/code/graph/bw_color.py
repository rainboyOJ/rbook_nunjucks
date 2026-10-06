# 二分图黑白染色：color[u] = 0 未染色、1 / 2 表示两侧；点编号 1..n（下标 0 不用）。
# bfs_color 只处理 start 所在连通块，并把整块染色；自环会让 u 与自己同色，判为非二分图。
# 想多次判定同一张图要先 init(n) 重置，否则 color 会残留上次结果。

from collections import deque

type Graph = list[list[int]]


class BipartiteChecker:
    """邻居染成 3 - color[u]（即 1 和 2 互换）；发现邻居与自己同色说明存在奇环。"""

    n: int
    graph: Graph
    color: list[int]

    def __init__(self, n: int = 0) -> None:
        self.init(n)

    def init(self, node_count: int) -> None:
        self.n = node_count
        self.graph = [[] for _ in range(node_count + 1)]
        self.color = [0] * (node_count + 1)

    def add_edge(self, u: int, v: int) -> None:
        self.graph[u].append(v)
        self.graph[v].append(u)

    def bfs_color(self, start: int) -> bool:
        self.color[start] = 1
        q = deque([start])
        while q:
            u = q.popleft()
            for v in self.graph[u]:
                if self.color[v] == 0:
                    self.color[v] = 3 - self.color[u]  # 1 与 2 互换，得到相反色
                    q.append(v)
                elif self.color[v] == self.color[u]:
                    return False  # 相邻同色，二分图判定失败
        return True

    def is_bipartite(self) -> bool:
        # 图可能不连通，每个未染色点都要单独起一次 BFS。
        for i in range(1, self.n + 1):
            if self.color[i] == 0 and not self.bfs_color(i):
                return False
        return True
