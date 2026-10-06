# 无向图边双连通分量（两遍法，自包含版）：第一遍 Tarjan 找桥，第二遍不走桥地染色。
# 调用契约：t = TarjanEBCC(); t.init(n); t.add_edge(u, v); t.solve(n);
#           结果 t.bcc_cnt 与 t.bcc_id[1..n]。
# 这里用父节点 fa 挡回头边（与 C++ 一致，重边会被误判为父边）；点编号 1..n。
# tarjan / dfs_color 递归深度最坏 n（1e5+），使用前需 sys.setrecursionlimit(1 << 20)。


class Edge:
    """无向边的有向存储：u 起点、v 终点、next 同起点的下一条边编号。"""

    __slots__ = ("u", "v", "next")

    def __init__(self, u: int, v: int, next: int) -> None:
        self.u = u
        self.v = v
        self.next = next


class TarjanEBCC:
    """is_bridge 与边编号一一对应；bcc_id[u] ∈ [1, bcc_cnt] 是染色后的分量编号。"""

    def __init__(self) -> None:
        self.n = 0
        self.e: list[Edge] = []
        self.head: list[int] = []
        self.e_cnt = 0
        self.dfn: list[int] = []
        self.low: list[int] = []
        self.timer = 0
        self.is_bridge: list[bool] = []
        self.bcc_id: list[int] = []
        self.bcc_cnt = 0

    def init(self, n: int) -> None:
        self.n = n
        self.e = []
        self.e_cnt = 0
        self.timer = 0
        self.bcc_cnt = 0
        self.head = [-1] * (n + 1)
        self.dfn = [0] * (n + 1)
        self.low = [0] * (n + 1)
        self.bcc_id = [0] * (n + 1)
        self.is_bridge = []  # 随 add_edge 增长，等价于 C++ 按最大边数清零

    def add_edge(self, u: int, v: int) -> None:
        self.e.append(Edge(u, v, self.head[u]))
        self.head[u] = self.e_cnt
        self.e_cnt += 1
        self.is_bridge.append(False)
        self.e.append(Edge(v, u, self.head[v]))
        self.head[v] = self.e_cnt
        self.e_cnt += 1
        self.is_bridge.append(False)

    def tarjan(self, u: int, fa: int) -> None:
        self.timer += 1
        self.dfn[u] = self.low[u] = self.timer
        i = self.head[u]
        while i != -1:
            v = self.e[i].v
            if v != fa:
                if self.dfn[v] == 0:
                    self.tarjan(v, u)
                    if self.low[v] < self.low[u]:
                        self.low[u] = self.low[v]
                    # 同割边：low[v] > dfn[u] 时该边是桥，正反两条边一起标记。
                    if self.low[v] > self.dfn[u]:
                        self.is_bridge[i] = True
                        self.is_bridge[i ^ 1] = True
                elif self.dfn[v] < self.low[u]:
                    self.low[u] = self.dfn[v]
            i = self.e[i].next

    def dfs_color(self, u: int, id: int) -> None:
        self.bcc_id[u] = id
        i = self.head[u]
        while i != -1:
            v = self.e[i].v
            # 不走桥，且对面还没染过色：同一个边双内部任意走。
            if not self.is_bridge[i] and not self.bcc_id[v]:
                self.dfs_color(v, id)
            i = self.e[i].next

    def solve(self, n: int) -> None:
        for i in range(1, n + 1):
            if self.dfn[i] == 0:
                self.tarjan(i, -1)
        for i in range(1, n + 1):
            if not self.bcc_id[i]:
                self.bcc_cnt += 1
                self.dfs_color(i, self.bcc_cnt)
