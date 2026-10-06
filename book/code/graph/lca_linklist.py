# 倍增 LCA：先往全局 e（链式前向星）里 add2 建好树，再 lca.init(n, root) 预处理。
# 调用契约：e.reset(); 对每条树边 e.add2(u, v); lca.init(n, root); 然后 lca.ask(u, v)。
# dfs 改显式栈：链状树深度可达 n，递归写法会爆 Python 栈；f 表按 n 动态分配，MAXLOG=20。

from collections import defaultdict

MAXLOG = 20  # 最大跳 2^20 步，支持 n 不超过 2^21 量级


class Edge:
    """链式前向星的边：v 终点、next 下一条出边编号（本文件只用这两个字段）。"""

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


class LCA:
    """f[u][i] 是 u 的 2^i 祖先（0 表示黑洞/不存在）；d[u] 是深度。"""

    def __init__(self) -> None:
        self.f: list[list[int]] = []
        self.d: list[int] = []

    def init(self, n: int, root: int) -> None:
        # f[0] 一整行留 0，这样 f[f[u][j-1]][j-1] 在祖先不存在时会落到 0。
        self.f = [[0] * (MAXLOG + 1) for _ in range(n + 1)]
        self.d = [0] * (n + 1)
        self.dfs(root, 0, 1)

        # 倍增预处理：f[u][j] = f[f[u][j-1]][j-1]（先跳 2^{j-1}，再跳 2^{j-1}）。
        for j in range(1, MAXLOG + 1):
            for i in range(1, n + 1):
                self.f[i][j] = self.f[self.f[i][j - 1]][j - 1]

    def dfs(self, root: int, fa: int, depth: int) -> None:
        # 树边才保证每点只入栈一次；e 里存的是无向边，用 fa 挡住回头路。
        stack = [(root, fa, depth)]
        while stack:
            u, p, dep = stack.pop()
            self.d[u] = dep
            self.f[u][0] = p
            i = e.h[u]
            while i != -1:
                edge = e[i]
                if edge.v != p:
                    stack.append((edge.v, u, dep + 1))
                i = edge.next

    def ask(self, u: int, v: int) -> int:
        # 1. 让 u 是较深的那个。
        if self.d[u] < self.d[v]:
            u, v = v, u

        # 2. u 上跳到与 v 同层：能跳且不越过 v 的深度才跳。
        for i in range(MAXLOG, -1, -1):
            if self.d[u] - (1 << i) >= self.d[v]:
                u = self.f[u][i]

        if u == v:
            return u

        # 3. 一起上跳，直到 LCA 的下一层；此时父亲即 LCA。
        for i in range(MAXLOG, -1, -1):
            if self.f[u][i] != self.f[v][i]:
                u = self.f[u][i]
                v = self.f[v][i]
        return self.f[u][0]


lca = LCA()
