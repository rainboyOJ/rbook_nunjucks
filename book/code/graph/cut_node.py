# Tarjan 求无向图割点。依赖模块级链式前向星全局 e（本文件内联了最小实现，完整版见 linklist.py）：
#   e.reset(); e.add2(u, v)   # 无向边双向各加一条
#   t = TarjanCut(); t.set(n); t.solve(); cuts = t.get_cuts()
# 非根割点条件 low[v] >= dfn[u]（取等号：v 回不到 u 的祖先）；根割点当且仅当有 ≥2 棵 DFS 子树。
# 点编号 1..n；dfs 递归深度最坏 n（1e5+），使用前需 sys.setrecursionlimit(1 << 20)。

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


class TarjanCut:
    """dfn/low 为时间戳；root 是当前 DFS 树的根，用来做根割点的子树数特判。"""

    def __init__(self) -> None:
        self.n = 0
        self.timer = 0
        self.dfn: list[int] = []
        self.low: list[int] = []
        self.is_cut: list[bool] = []
        self.root = 0

    def set(self, _n: int) -> None:
        self.n = _n
        self.timer = 0
        self.dfn = [0] * (_n + 1)
        self.low = [0] * (_n + 1)
        self.is_cut = [False] * (_n + 1)
        self.root = 0

    def dfs(self, u: int, fa: int = -1) -> None:
        self.timer += 1
        self.dfn[u] = self.low[u] = self.timer
        child = 0  # 统计 u 在 DFS 树里的子节点数，只有根节点需要

        i = e.h[u]
        while i != -1:
            v = e[i].v
            if v != fa:  # 不走父子边（重边情形需改用边编号判断，这里沿用 C++ 写法）
                if self.dfn[v] == 0:  # 树边
                    child += 1
                    self.dfs(v, u)
                    if self.low[v] < self.low[u]:
                        self.low[u] = self.low[v]
                    # v 回不到 u 的祖先，删除 u 就会把 v 子树切出去。
                    if self.low[v] >= self.dfn[u] and u != self.root:
                        self.is_cut[u] = True
                elif self.dfn[v] < self.dfn[u]:
                    # 返祖边只能取 dfn[v]：边 (u, v) 不代表 u 能借 v 继续往上跳。
                    if self.dfn[v] < self.low[u]:
                        self.low[u] = self.dfn[v]
            i = e[i].next

        # 根节点没有祖先，只有 ≥2 棵子树时才是割点。
        if u == self.root and child > 1:
            self.is_cut[u] = True

    def solve(self) -> None:
        for i in range(1, self.n + 1):
            if self.dfn[i] == 0:
                self.root = i  # 每个连通块单独确定根
                self.dfs(i, 0)  # fa = 0 是 1..n 之外的哨兵，等价于「没有父亲」

    def get_cuts(self) -> list[int]:
        return [i for i in range(1, self.n + 1) if self.is_cut[i]]
