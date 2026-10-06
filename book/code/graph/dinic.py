# Dinic 最大流：add_edge 同时加正向边（容量 cap）和反向边（容量 0），编号成对，i ^ 1 是配对边。
# 点编号 1..n（下标 0 不用）；capacity 是 long long，在 Python 里是任意精度 int，不会溢出。
# max_flow 前必须 add_edge 建好图；dfs 的 limit 用 LLONG_MAX 复刻 C++ 的 numeric_limits<long long>::max()。
# dfs 递归深度最坏 n（1e5+），使用前需 sys.setrecursionlimit(1 << 20)。

LLONG_MAX = (1 << 63) - 1  # C++ long long 的最大值，作为「不限流量」的初值


class Edge:
    """残留网络的一条弧：to 终点、next 同起点的下一条弧、capacity 剩余容量。"""

    __slots__ = ("to", "next", "capacity")

    def __init__(self, to: int, next: int, capacity: int) -> None:
        self.to = to
        self.next = next
        self.capacity = capacity


class Dinic:
    """head 是每个点的第一条弧；current 是当前弧优化的游标；level 是 BFS 分层。"""

    def __init__(self, n: int = 0, max_edges: int = 0) -> None:
        self.init(n, max_edges)

    def init(self, n: int, max_edges: int = 0) -> None:
        self.node_count = n
        self.edges: list[Edge] = []
        # max_edges 只对应 C++ 的 reserve，Python 列表动态扩容，保留参数仅为接口对齐。
        self.head = [-1] * (n + 1)
        self.level = [0] * (n + 1)
        self.current = [0] * (n + 1)

    def add_edge(self, u: int, v: int, capacity: int) -> None:
        self.edges.append(Edge(v, self.head[u], capacity))
        self.head[u] = len(self.edges) - 1
        self.edges.append(Edge(u, self.head[v], 0))  # 反向边，初始容量 0
        self.head[v] = len(self.edges) - 1

    def bfs(self, source: int, sink: int) -> bool:
        """按残留容量做分层；返回 sink 是否可达。"""
        self.level = [-1] * (self.node_count + 1)
        self.level[source] = 0
        q = [source]
        head = 0
        while head < len(q):
            u = q[head]
            head += 1
            i = self.head[u]
            while i != -1:
                v = self.edges[i].to
                if self.edges[i].capacity > 0 and self.level[v] == -1:
                    self.level[v] = self.level[u] + 1
                    q.append(v)
                i = self.edges[i].next
        return self.level[sink] != -1

    def dfs(self, u: int, sink: int, limit: int) -> int:
        if u == sink or limit == 0:
            return limit

        flow = 0
        i = self.current[u]
        while i != -1:
            edge = self.edges[i]
            v = edge.to
            if edge.capacity <= 0 or self.level[v] != self.level[u] + 1:
                i = edge.next
                continue

            pushed = self.dfs(v, sink, min(limit, edge.capacity))
            if pushed == 0:
                i = edge.next
                continue

            edge.capacity -= pushed
            self.edges[i ^ 1].capacity += pushed
            flow += pushed
            limit -= pushed
            if limit == 0:
                break
            i = edge.next

        # 回写当前弧（对应 C++ 的 int& i = current[u]）；本层已无增广路则把 u 从分层图删掉。
        self.current[u] = i
        if flow == 0:
            self.level[u] = -1
        return flow

    def max_flow(self, source: int, sink: int) -> int:
        answer = 0
        while self.bfs(source, sink):
            self.current = self.head[:]  # current = head，每次重新从各点第一条弧开始
            while True:
                flow = self.dfs(source, sink, LLONG_MAX)
                if flow == 0:
                    break
                answer += flow
        return answer
