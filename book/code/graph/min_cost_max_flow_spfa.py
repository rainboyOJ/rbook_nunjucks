# 最小费用最大流（SPFA 连续最短路 SSP）：点编号 1..n，add_edge 自动补反向边。
# 反向边容量 0、费用 -cost，保证可以撤销之前的流；所有费用先取负再累加会抵消。
# INF=10**9：C++ 用 int 存 dist，加费用可能溢出；Python int 任意精度，不会溢出。

from collections import deque


class Edge:
    """残量网络边：to 终点、rev 反向边在 g[to] 里的下标、cap 剩余容量、cost 单位费用。"""

    __slots__ = ("to", "rev", "cap", "cost")

    def __init__(self, to: int, rev: int, cap: int, cost: int) -> None:
        self.to = to
        self.rev = rev
        self.cap = cap
        self.cost = cost


class MinCostMaxFlow:
    """g[u] 是 u 的出边表；dist/prev_v/prev_e 记录 SPFA 的最短路与路径。"""

    INF = 10**9

    def __init__(self, n: int) -> None:
        self.n = n
        self.g: list[list[Edge]] = [[] for _ in range(n + 1)]
        self.dist = [0] * (n + 1)
        self.prev_v = [0] * (n + 1)
        self.prev_e = [0] * (n + 1)

    def add_edge(self, from_: int, to: int, cap: int, cost: int) -> None:
        # from 是 Python 关键字，参数改名 from_，语义与 C++ 的 from 一致。
        # 反向边 rev 记的是对方在各自表中的下标，加完两条边后下标才对齐。
        forward = Edge(to, len(self.g[to]), cap, cost)
        backward = Edge(from_, len(self.g[from_]), 0, -cost)
        self.g[from_].append(forward)
        self.g[to].append(backward)

    def spfa(self, s: int, t: int) -> bool:
        """按费用做 SPFA 求 s->t 的最短路；存在通路时返回 True 并留下路径。"""
        for i in range(1, self.n + 1):
            self.dist[i] = self.INF
        in_queue = [False] * (self.n + 1)
        q: deque[int] = deque([s])
        self.dist[s] = 0
        in_queue[s] = True

        while q:
            u = q.popleft()
            in_queue[u] = False
            for i, edge in enumerate(self.g[u]):
                if edge.cap <= 0:
                    continue
                if self.dist[edge.to] > self.dist[u] + edge.cost:
                    self.dist[edge.to] = self.dist[u] + edge.cost
                    self.prev_v[edge.to] = u
                    self.prev_e[edge.to] = i
                    if not in_queue[edge.to]:
                        in_queue[edge.to] = True
                        q.append(edge.to)

        return self.dist[t] != self.INF

    def min_cost_max_flow(self, s: int, t: int) -> tuple[int, int]:
        """返回 (最大流, 最小费用)；s 到不了 t 时是 (0, 0)。"""
        flow = 0
        cost = 0

        while self.spfa(s, t):
            pushed = self.INF
            v = t
            while v != s:
                edge = self.g[self.prev_v[v]][self.prev_e[v]]
                pushed = min(pushed, edge.cap)
                v = self.prev_v[v]

            flow += pushed
            cost += pushed * self.dist[t]

            v = t
            while v != s:
                edge = self.g[self.prev_v[v]][self.prev_e[v]]
                edge.cap -= pushed
                self.g[v][edge.rev].cap += pushed
                v = self.prev_v[v]

        return flow, cost
