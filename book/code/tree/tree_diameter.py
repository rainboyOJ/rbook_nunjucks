# 依赖模块级全局邻接表 tree：使用前先 tree = [[] for _ in range(n + 1)]，
# 再加带权边 tree[u].append(Edge(v, w)) 和 tree[v].append(Edge(u, w))（无向，双向各加一条）。
# 两次 BFS 找最远点求直径，要求边权非负；dis 用 -1 当未访问标记，节点编号 1..n。
# C++ 的 ll 在 Python 里就是 int，任意精度，无需担心溢出。

from collections import deque
from typing import NamedTuple


class Edge(NamedTuple):
    to: int
    w: int


type Graph = list[list[Edge]]

tree: Graph = []


class TreeDiameter:
    """BFS 版：任意点出发找最远点 a，再从 a 找最远点 b，a-b 即直径。"""

    n: int
    a: int
    b: int
    ans: int
    dis: list[int]

    def __init__(self, n: int) -> None:
        self.n = n
        self.a = 0
        self.b = 0
        self.ans = 0
        self.dis = [0] * (n + 1)

    def farthest(self, s: int) -> int:
        """从 s 出发 BFS，返回距离最远的节点编号（并列时取先访问到的）。"""
        self.dis = [-1] * (self.n + 1)
        q: deque[int] = deque([s])
        self.dis[s] = 0
        far = s

        while q:
            u = q.popleft()
            # 严格大于：并列时保留先出队的那个，与 C++ 一致。
            if self.dis[u] > self.dis[far]:
                far = u
            for edge in tree[u]:
                v = edge.to
                if self.dis[v] != -1:
                    continue
                self.dis[v] = self.dis[u] + edge.w
                q.append(v)
        return far

    def solve(self) -> None:
        self.a = self.farthest(1)
        self.b = self.farthest(self.a)
        self.ans = self.dis[self.b]
