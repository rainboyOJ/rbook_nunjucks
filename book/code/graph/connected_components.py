# 无向图连通块划分：component[u] 为所属块编号（1..component_count），0 表示未访问。
# 点编号 1..n（下标 0 不用）；add_edge 双向加边。
# dfs 递归深度最坏 n（1e5+ 长链），使用前需 sys.setrecursionlimit(1 << 20)，或自行改栈式迭代。

type Adj = list[list[int]]


class Graph:
    """component_count 只在 find_components 里自增，每个未访问点开一个新块。"""

    n: int
    adj: Adj
    component: list[int]
    component_count: int

    def __init__(self, n: int) -> None:
        self.n = n
        self.adj = [[] for _ in range(n + 1)]
        self.component = [0] * (n + 1)
        self.component_count = 0

    def add_edge(self, u: int, v: int) -> None:
        self.adj[u].append(v)
        self.adj[v].append(u)

    def dfs(self, u: int, id: int) -> None:
        self.component[u] = id
        for v in self.adj[u]:
            if self.component[v]:
                continue  # 已有块编号，说明本块内已经访问过
            self.dfs(v, id)

    def find_components(self) -> None:
        for u in range(1, self.n + 1):
            if self.component[u]:
                continue
            self.component_count += 1
            self.dfs(u, self.component_count)
