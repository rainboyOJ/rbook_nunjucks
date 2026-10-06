# 依赖模块级全局邻接表 tree：使用前先 tree = [[] for _ in range(n + 1)]，
# 再加带权边 tree[u].append(Edge(v, w)) 和 tree[v].append(Edge(u, w))（无向，双向各加一条）。
# 树上 DP 求直径，要求边权非负；f[u] = 从 u 往下走的最长链，节点编号 1..n。
# dfs 递归深度最坏 n（1e5+），使用前需 sys.setrecursionlimit(1 << 20)。
# C++ 的 ll 在 Python 里就是 int，任意精度，无需担心溢出。

from typing import NamedTuple


class Edge(NamedTuple):
    to: int
    w: int


type Graph = list[list[Edge]]

tree: Graph = []


class TreeDiameterDP:
    """扫描 u 的儿子时，用「已处理儿子的最长链 + 当前儿子的链」在 u 拼接更新直径。"""

    n: int
    ans: int
    f: list[int]

    def __init__(self, n: int) -> None:
        self.n = n
        self.ans = 0
        self.f = [0] * (n + 1)

    def solve(self) -> None:
        self.dfs(1, 0)

    def dfs(self, u: int, parent: int) -> None:
        for edge in tree[u]:
            v = edge.to
            if v == parent:
                continue
            self.dfs(v, u)

            # 过 u 的两条链拼接（f[u] 还是 0 时就是单链），再并入当前儿子的链。
            if self.f[u] + self.f[v] + edge.w > self.ans:
                self.ans = self.f[u] + self.f[v] + edge.w
            if self.f[v] + edge.w > self.f[u]:
                self.f[u] = self.f[v] + edge.w
