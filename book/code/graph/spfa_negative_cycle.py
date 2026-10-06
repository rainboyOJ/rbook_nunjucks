# SPFA 判负环：把所有点先压入队列（相当于加一个超级源点，各点初值 0），
# 某点被松弛次数 relax_count[v] >= n 就说明存在负环；点编号 1..n。
# 多次调用 has_negative_cycle 不会自动清零（与 C++ 一致），要重算请新建对象。
# C++ 用 long long 存距离防溢出；Python int 任意精度，不会溢出。

from collections import deque

type WeightedAdj = list[list[tuple[int, int]]]  # graph[u]：若干 (终点 v, 边权 w)


class NegativeCycleSPFA:
    """dist 初值全 0；relax_count 统计每个点被松弛的次数。"""

    def __init__(self, n: int) -> None:
        self.n = n
        self.graph: WeightedAdj = [[] for _ in range(n + 1)]
        self.dist = [0] * (n + 1)
        self.relax_count = [0] * (n + 1)
        self.in_queue = [False] * (n + 1)

    def add_edge(self, u: int, v: int, w: int) -> None:
        self.graph[u].append((v, w))

    def has_negative_cycle(self) -> bool:
        q: deque[int] = deque(range(1, self.n + 1))
        for i in range(1, self.n + 1):
            self.in_queue[i] = True

        while q:
            u = q.popleft()
            self.in_queue[u] = False
            for v, w in self.graph[u]:
                if self.dist[v] > self.dist[u] + w:
                    self.dist[v] = self.dist[u] + w
                    self.relax_count[v] = self.relax_count[u] + 1
                    if self.relax_count[v] >= self.n:
                        return True
                    if not self.in_queue[v]:
                        q.append(v)
                        self.in_queue[v] = True

        return False
