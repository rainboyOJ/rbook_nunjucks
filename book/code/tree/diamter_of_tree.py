# 依赖模块级链式前向星全局量 e（C++ 里由外部提供）：调用前先 e.init(n)，再加边 e.add_edge(u, v, w)。
# e.h[u] 是 u 的第一条边下标，-1 表示没有（C++ 用 ~i 判 -1）；e[i].v / e[i].w / e[i].next 是终点、边权、下一条边。
# e 的下标从 1 开始，e[0] 是占位边，对应 C++ 的 e[++cnt]。
# 类名沿用 C++ 的 tree_diamter；C++ 字段 from 是 Python 关键字，故写作 from_。
# C++ 的 dis 与边权都是 int，路径和超过 2^31 - 1 会溢出；Python int 任意精度不会，
# 因此大权值下结果可能与 C++ 不同。
# dfs_diamter 递归深度最坏 n（1e5+），使用前需 sys.setrecursionlimit(1 << 20)。


class Edge:
    """链式前向星的一条边：v 是终点，w 是边权，next 是下一条边下标。"""

    __slots__ = ("v", "w", "next")

    def __init__(self, v: int = 0, w: int = 0, next: int = -1) -> None:
        self.v = v
        self.w = w
        self.next = next


class LinkList:
    """链式前向星：h 按节点编号 1..n 开，_edges 按加边顺序追加。"""

    __slots__ = ("h", "_edges")

    def __init__(self) -> None:
        self.h = [-1]
        self._edges = [Edge()]

    def init(self, n: int) -> None:
        """重置为 n 个点、没有边的空图。"""
        self.h = [-1] * (n + 1)
        self._edges = [Edge()]

    def add_edge(self, u: int, v: int, w: int = 1) -> None:
        """加无向边：头插两条有向边，和 C++ 的 e[++cnt] = {v, h[u], w} 一致。"""
        self._edges.append(Edge(v, w, self.h[u]))
        self.h[u] = len(self._edges) - 1
        self._edges.append(Edge(u, w, self.h[v]))
        self.h[v] = len(self._edges) - 1

    def __getitem__(self, i: int) -> Edge:
        return self._edges[i]


e = LinkList()


class tree_diamter:
    """两次 DFS 求直径：第一次从起点找最远点 st，第二次从 st 找 ed，dis[st] 就是直径长度。"""

    dis: list[int]
    next: list[int]
    from_: list[int]
    st: int
    ed: int

    def __init__(self, n: int) -> None:
        # C++ 用模板参数 N 定长开数组，Python 按点数 n 动态分配。
        self.dis = [0] * (n + 1)
        # C++ 声明了 next 数组但模板内未使用，保留以对齐结构。
        self.next = [0] * (n + 1)
        self.from_ = [0] * (n + 1)
        self.st = 0
        self.ed = 0

    def dfs_diamter(self, u: int, fa: int) -> int:
        """返回以 u 为根的子树里离 u 最远的节点；同时填 dis[u] 与 from_[u]。"""
        self.dis[u] = 0
        tu = u
        i = e.h[u]
        while i != -1:
            v = e[i].v
            if v != fa:
                x = self.dfs_diamter(v, u)  # 从 v 开始的最远点
                length = self.dis[v] + e[i].w
                if length > self.dis[u]:
                    self.dis[u] = length
                    tu = x
                    self.from_[u] = v
            i = e[i].next
        return tu

    def two_time_dfs(self, start_node: int = 1) -> None:
        for i in range(len(self.dis)):
            self.dis[i] = 0
        self.st = self.dfs_diamter(start_node, 0)
        for i in range(len(self.dis)):
            self.dis[i] = 0
        self.ed = self.dfs_diamter(self.st, 0)
