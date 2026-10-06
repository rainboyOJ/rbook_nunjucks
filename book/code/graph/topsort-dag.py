# Kahn 拓扑排序：点编号 1..n，add_edge 维护入度 indeg；返回的 order 长度小于 n 说明有环。
# kahn 内部拷贝一份入度，不破坏原图的 indeg，可以反复调用。
# 队列里入度 0 的点按编号从小到大、同层按加边顺序处理，但拓扑序本身不是唯一契约。

from collections import deque


class TopologicalSort:
    """graph 是出边邻接表，indeg[v] 是 v 的入度（含重边，重边要重复计数）。"""

    def __init__(self, n: int) -> None:
        self.n = n
        self.graph: list[list[int]] = [[] for _ in range(n + 1)]
        self.indeg = [0] * (n + 1)

    def add_edge(self, u: int, v: int) -> None:
        self.graph[u].append(v)
        self.indeg[v] += 1

    def kahn(self) -> list[int]:
        deg = self.indeg[:]  # 拷贝，保证原入度不被修改
        q: deque[int] = deque(i for i in range(1, self.n + 1) if deg[i] == 0)
        order: list[int] = []

        while q:
            u = q.popleft()
            order.append(u)
            for v in self.graph[u]:
                deg[v] -= 1
                if deg[v] == 0:
                    q.append(v)

        return order
