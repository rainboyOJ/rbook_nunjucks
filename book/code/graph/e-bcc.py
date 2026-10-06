# Tarjan 求无向图边双连通分量（e-BCC）。依赖模块级链式前向星全局 e（内联最小实现，完整版见 linklist.py）：
#   e.reset(); e.add2(u, v)
#   t = TarjanEBCC(); t.set(n); t.solve(); t.bcc_cnt / t.bcc_id[1..n]
# 与割边共用 low 数组：low[u] == dfn[u] 时弹栈，栈中 u 及其上面的点构成一个 e-BCC。
# 这里用父节点 fa 挡回头边（与 C++ 一致，重边会被误判为父边）；点编号 1..n。
# dfs 递归深度最坏 n（1e5+），使用前需 sys.setrecursionlimit(1 << 20)。

from collections import defaultdict


class Edge:
    """链式前向星的边：v 终点、next 同起点的下一条边编号。"""

    __slots__ = ("u", "v", "w", "next")

    def __init__(self, u: int, v: int, w: int, next: int) -> None:
        self.u = u
        self.v = v
        self.w = w
        self.next = next


class LinkList:
    """与 linklist.py 的 linkList 等价的最小实现，保证本文件可独立复制使用。"""

    def __init__(self) -> None:
        self.reset()

    def reset(self) -> None:
        self.edge_cnt = 0
        self.e: list[Edge] = []
        self.h: defaultdict[int, int] = defaultdict(lambda: -1)

    def add(self, u: int, v: int, w: int = 0) -> None:
        self.e.append(Edge(u, v, w, self.h[u]))
        self.h[u] = self.edge_cnt
        self.edge_cnt += 1

    def add2(self, u: int, v: int, w: int = 0) -> None:
        self.add(u, v, w)
        self.add(v, u, w)

    def __getitem__(self, i: int) -> Edge:
        return self.e[i]


e = LinkList()


class TarjanEBCC:
    """st 保存当前 DFS 路径上的点；bcc_id[u] ∈ [1, bcc_cnt] 表示 u 所属的 e-BCC。"""

    def __init__(self) -> None:
        self.n = 0
        self.timer = 0
        self.st: list[int] = []
        self.dfn: list[int] = []
        self.low: list[int] = []
        self.bcc_cnt = 0
        self.bcc_id: list[int] = []

    def set(self, _n: int) -> None:
        self.n = _n
        self.timer = 0
        self.bcc_cnt = 0
        self.dfn = [0] * (_n + 1)
        self.low = [0] * (_n + 1)
        self.bcc_id = [0] * (_n + 1)
        self.st = []

    def dfs(self, u: int, fa: int) -> None:
        self.timer += 1
        self.dfn[u] = self.low[u] = self.timer
        self.st.append(u)

        i = e.h[u]
        while i != -1:
            v = e[i].v
            if v != fa:  # 无向图核心：不走回头路
                if self.dfn[v] == 0:
                    self.dfs(v, u)
                    if self.low[v] < self.low[u]:
                        self.low[u] = self.low[v]
                elif self.dfn[v] < self.low[u]:
                    self.low[u] = self.dfn[v]
            i = e[i].next

        # low[u] == dfn[u]：u 是所在边双的「根」，栈顶到 u 的点同属一个分量。
        if self.low[u] == self.dfn[u]:
            self.bcc_cnt += 1
            while True:
                node = self.st.pop()
                self.bcc_id[node] = self.bcc_cnt
                if node == u:
                    break

    def solve(self) -> None:
        for i in range(1, self.n + 1):
            if self.dfn[i] == 0:
                self.dfs(i, 0)  # fa = 0 是 1..n 之外的哨兵
