# 有向图 Tarjan 求强连通分量：先把有向边 e.add(u, v) 加进全局 e，再 scc.set(n)、scc.solve()。
# 结果在 scc.scc_id[u]（所属 SCC 编号，从 1 开始）与 scc.scc_cnt。
# dfs 改显式栈：有向图可以是 1e5 长链，递归写法会爆 Python 栈。

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

    def __getitem__(self, i: int) -> Edge:
        return self.e[i]


e = LinkList()


class TarjanScc:
    """dfn/low 是时间戳，st 是 Tarjan 栈，in_stack[v] 表示 v 还在栈中。"""

    def __init__(self) -> None:
        self.n = 0
        self.timer = 0
        self.st: list[int] = []
        self.in_stack: list[bool] = []
        self.dfn: list[int] = []
        self.low: list[int] = []
        self.scc_id: list[int] = []
        self.scc_cnt = 0

    def set(self, _n: int) -> None:
        """按点数 n 重新分配数组并清零（C++ 里是 memset 定长数组）。"""
        self.n = _n
        self.timer = 0
        self.scc_cnt = 0
        self.dfn = [0] * (_n + 1)
        self.low = [0] * (_n + 1)
        self.scc_id = [0] * (_n + 1)
        self.in_stack = [False] * (_n + 1)
        self.st = []

    def dfs(self, start: int) -> None:
        """从 start 出发做 Tarjan；用显式栈模拟递归，回溯时更新父节点 low。"""
        self.timer += 1
        self.dfn[start] = self.low[start] = self.timer
        self.st.append(start)
        self.in_stack[start] = True
        # 栈帧 (节点, 下一条待处理出边编号)
        call: list[tuple[int, int]] = [(start, e.h[start])]

        while call:
            u, i = call[-1]
            advanced = False
            while i != -1:
                v = e[i].v
                i = e[i].next
                if self.dfn[v] == 0:
                    call[-1] = (u, i)
                    self.timer += 1
                    self.dfn[v] = self.low[v] = self.timer
                    self.st.append(v)
                    self.in_stack[v] = True
                    call.append((v, e.h[v]))
                    advanced = True
                    break
                if self.in_stack[v]:
                    # 返祖边：只能回到栈中的点，用 dfn[v] 更新 low[u]。
                    self.low[u] = min(self.low[u], self.dfn[v])
            if advanced:
                continue

            call.pop()
            if self.low[u] == self.dfn[u]:
                self.scc_cnt += 1
                while True:
                    v = self.st.pop()
                    self.in_stack[v] = False
                    self.scc_id[v] = self.scc_cnt
                    if v == u:
                        break
            if call:
                # 对应递归返回后 low[父] = min(low[父], low[子])。
                pu = call[-1][0]
                self.low[pu] = min(self.low[pu], self.low[u])

    def solve(self) -> None:
        for i in range(1, self.n + 1):
            if self.dfn[i] == 0:
                self.dfs(i)
