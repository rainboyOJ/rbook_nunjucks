# 依赖模块级全局邻接表 tree：使用前先 tree = [[] for _ in range(n + 1)]，
# 再加带权边 tree[u].append(Edge(v, w)) 和 tree[v].append(Edge(u, w))（无向，双向各加一条）。
# 两次最远点搜索求直径并还原路径，要求边权非负；节点编号 1..n。
# C++ 的 ll 在 Python 里就是 int，任意精度，无需担心溢出。

from typing import NamedTuple


class Edge(NamedTuple):
    to: int
    w: int


type Graph = list[list[Edge]]

tree: Graph = []


class TreeDiameterDFS:
    """两遍 DFS 求直径，并记录 a -> b 的路径。"""

    n: int
    a: int
    b: int
    ans: int
    dis: list[int]
    parent: list[int]
    path: list[int]

    def __init__(self, n: int) -> None:
        self.n = n
        self.a = 0
        self.b = 0
        self.ans = 0
        self.dis = [0] * (n + 1)
        self.parent = [0] * (n + 1)
        self.path = []

    def dfs(self, u: int, fa: int, d: int) -> None:
        """迭代 DFS：栈里放 (节点, 父亲, 到起点的距离)，避免深树爆栈。"""
        stack: list[tuple[int, int, int]] = [(u, fa, d)]
        while stack:
            node, father, dist = stack.pop()
            self.dis[node] = dist
            self.parent[node] = father
            for edge in tree[node]:
                if edge.to == father:
                    continue
                stack.append((edge.to, node, dist + edge.w))

    def farthest(self, s: int) -> int:
        self.dfs(s, 0, 0)
        far = s
        for u in range(1, self.n + 1):
            # 严格大于：并列时取编号小的，与 C++ 顺序扫描一致。
            if self.dis[u] > self.dis[far]:
                far = u
        return far

    def build_path(self) -> None:
        self.path = []
        u = self.b
        while u != 0:
            self.path.append(u)
            if u == self.a:
                break
            u = self.parent[u]
        self.path.reverse()

    def solve(self) -> None:
        self.a = self.farthest(1)
        self.b = self.farthest(self.a)
        self.ans = self.dis[self.b]
        self.build_path()
