from typing import NamedTuple


class Edge(NamedTuple):
    u: int
    v: int
    w: int


class DSU:
    def __init__(self, n: int = 0) -> None:
        self.init(n)

    def init(self, n: int) -> None:
        self.parent = list(range(n + 1))
        self.size = [1] * (n + 1)

    def find(self, x: int) -> int:
        # 迭代路径压缩，不依赖 Python 的递归深度。
        root = x
        while self.parent[root] != root:
            root = self.parent[root]
        while self.parent[x] != root:
            self.parent[x], x = root, self.parent[x]
        return root

    def merge(self, a: int, b: int) -> bool:
        a, b = self.find(a), self.find(b)
        if a == b:
            return False
        if self.size[a] < self.size[b]:
            a, b = b, a
        self.parent[b] = a
        self.size[a] += self.size[b]
        return True


def kruskal(n: int, edges: list[Edge]) -> int | None:
    """点编号 1..n，n >= 1；返回 MST 权值，不连通时返回 None。"""
    order = sorted(edges, key=lambda edge: edge.w)
    dsu = DSU(n)
    answer = selected = 0
    for edge in order:
        # merge 返回 True，才说明这条边连接了两个不同的块。
        if not dsu.merge(edge.u, edge.v):
            continue
        answer += edge.w
        selected += 1
        if selected == n - 1:
            break
    return answer if selected == n - 1 else None
