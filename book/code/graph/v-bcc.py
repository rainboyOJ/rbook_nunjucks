# 无向图 Tarjan 求点双连通分量（v-BCC），顺便标出割点。
# 调用契约：e.reset(); 每条无向边 e.add2(u, v); bcc.set(n); bcc.solve();
# 结果：bcc.bcc[1..bcc_cnt] 是各点双的点集，割点看 bcc.is_cut[u]。
# dfs 改显式栈：无向图可以是 1e5 长链，递归写法会爆 Python 栈；割点可属于多个 BCC，故用点集列表。

from collections import defaultdict


class Edge:
    """链式前向星的边：v 终点、next 下一条出边编号。"""

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


class TarjanBCC:
    """dfn/low 是时间戳，st 存已访问点；bcc 按下标 1..bcc_cnt 存各分量的点集。"""

    def __init__(self) -> None:
        self.n = 0
        self.timer = 0
        self.st: list[int] = []
        self.dfn: list[int] = []
        self.low: list[int] = []
        self.bcc_cnt = 0
        self.is_cut: list[bool] = []
        self.root = 0
        self.bcc: list[list[int]] = []

    def set(self, _n: int) -> None:
        """按点数 n 重新分配并清零；bcc 下标 1..n 够用（n 点图至多 n-1 个点双）。"""
        self.n = _n
        self.timer = 0
        self.bcc_cnt = 0
        self.dfn = [0] * (_n + 1)
        self.low = [0] * (_n + 1)
        self.is_cut = [False] * (_n + 1)
        self.st = []
        self.bcc = [[] for _ in range(_n + 1)]
        self.root = 0

    def dfs(self, root: int, fa: int) -> None:
        """从 root 出发做点双；fa 挡住直接走回父节点，root 用于根割点特判。"""
        self.timer += 1
        self.dfn[root] = self.low[root] = self.timer
        self.st.append(root)
        # 栈帧 [u, fa, 下一条待处理出边编号, 已完成的子节点数, 是否已初始化]
        stack: list[list[int]] = [[root, fa, e.h[root], 0, 1]]

        while stack:
            frame = stack[-1]
            u = frame[0]
            i = frame[2]
            advanced = False
            while i != -1:
                v = e[i].v
                i = e[i].next
                if v == frame[1]:
                    continue  # 无向图核心：不走回头边
                if self.dfn[v] == 0:
                    frame[2] = i
                    self.timer += 1
                    self.dfn[v] = self.low[v] = self.timer
                    self.st.append(v)
                    stack.append([v, u, e.h[v], 0, 1])
                    advanced = True
                    break
                if self.dfn[v] < self.dfn[u]:
                    # 返祖边：用 dfn[v] 更新 low[u]（dfn[v] > dfn[u] 的是前向边，忽略）
                    if self.dfn[v] < self.low[u]:
                        self.low[u] = self.dfn[v]
            if advanced:
                continue

            # u 的邻边处理完，弹栈；此时父节点刚“递归返回”，做子节点收尾工作。
            stack.pop()
            if stack:
                p = stack[-1]
                pu = p[0]
                p[3] += 1  # 父节点的子节点计数 child++
                if pu != self.root and self.low[u] >= self.dfn[pu]:
                    self.is_cut[pu] = True
                if self.low[u] < self.low[pu]:
                    self.low[pu] = self.low[u]
                if self.low[u] >= self.dfn[pu]:
                    # v 子树无法绕过 pu 回到更早祖先：弹到 v 为止构成一个点双。
                    self.bcc_cnt += 1
                    comp: list[int] = []
                    while True:
                        node = self.st.pop()
                        comp.append(node)
                        if node == u:
                            break
                    comp.append(pu)  # pu 也属于该点双，但不出栈（它可能属于多个分量）
                    self.bcc[self.bcc_cnt] = comp
            elif u == self.root and frame[3] > 1:
                self.is_cut[u] = True  # 根是割点当且仅当有至少两棵子树

    def solve(self) -> None:
        for i in range(1, self.n + 1):
            if self.dfn[i] == 0 and e.h[i] != -1:  # 孤立点不产生点双，直接跳过
                self.root = i
                self.dfs(i, 0)

    def cut_cnt(self) -> int:
        return sum(1 for i in range(1, self.n + 1) if self.is_cut[i])
