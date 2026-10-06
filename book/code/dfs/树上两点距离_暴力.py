# 依赖模块级链式前向星全局量 e（C++ 里由外部提供）：调用前先 e.init(n)，再加边 e.add_edge(u, v, w)。
# e.h[u] 是 u 的第一条边下标，-1 表示没有；e[i].v / e[i].w / e[i].next 是终点、边权、下一条边。
# 返回值：u 到 aim 的路径长度；aim 不在 u 的子树里时返回 -1（与 C++ 的哨兵一致）。
# C++ 的边权与返回值都是 int，路径和超过 2^31 - 1 会溢出；Python int 任意精度不会，
# 因此大权值下结果可能与 C++ 不同。
# 函数递归深度最坏 n（1e5+），使用前需 sys.setrecursionlimit(1 << 20)。


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


def dfs_get_two_node_len(u: int, fa: int, aim: int) -> int:
    """求 u -> aim 的距离，沿 u 的子树（避开父亲 fa）找；找不到返回 -1。"""
    if u == aim:
        return 0
    i = e.h[u]
    while i != -1:
        v = e[i].v
        if v != fa:
            length = dfs_get_two_node_len(v, u, aim)
            if length != -1:
                return length + e[i].w
        i = e[i].next
    return -1
