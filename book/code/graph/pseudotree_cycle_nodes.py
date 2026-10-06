# 基环树找环上的点：无向图拓扑剥叶子，剩下的 in_cycle 点就是环上点。
# 只有基环树（n 个点 n 条边且连通）才有唯一环；点数 n 由构造函数给定，点编号 1..n。
# 每个点的度数含重边与自环，剥到 degree<=1 就入队，队列里的点可能重复入队、靠 in_cycle 去重。

from collections import deque


class PseudotreeCycle:
    """graph 是邻接表；degree 是当前剩余度数；in_cycle[u] 为 False 表示 u 已被剥掉。"""

    def __init__(self, n: int) -> None:
        self.n = n
        self.graph: list[list[int]] = [[] for _ in range(n + 1)]
        self.degree = [0] * (n + 1)
        self.in_cycle = [True] * (n + 1)

    def add_edge(self, u: int, v: int) -> None:
        self.graph[u].append(v)
        self.graph[v].append(u)
        self.degree[u] += 1
        self.degree[v] += 1

    def find_cycle_nodes(self) -> list[int]:
        """返回环上所有点，按编号升序；无环图返回空列表。"""
        q: deque[int] = deque()
        for i in range(1, self.n + 1):
            if self.degree[i] <= 1:
                q.append(i)

        while q:
            u = q.popleft()
            if not self.in_cycle[u]:
                continue  # 可能被多次入队，剥过一次就跳过
            self.in_cycle[u] = False
            for v in self.graph[u]:
                if not self.in_cycle[v]:
                    continue
                self.degree[v] -= 1
                if self.degree[v] == 1:  # 从 2 降到 1 才入队，避免重复
                    q.append(v)

        return [i for i in range(1, self.n + 1) if self.in_cycle[i]]
