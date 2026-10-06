# Dinic 最大流：基于模块级链式前向星 e（对应 C++ 全局 linkList），i ^ 1 是配对的反向边。
# 点编号 0..n；容量是 long long，Python int 任意精度不会溢出。先 init(n) 再 add_edge。
# dfs 递归深度最坏可达 n（1e5+），使用前需 sys.setrecursionlimit(1 << 20)。

from collections import defaultdict, deque

LLONG_MAX = (1 << 63) - 1  # C++ long long 上限，作为「不限流量」的初值

type IntList = list[int]  # 与点一一对应的定长数组，下标 0..n+4


class Edge:
    """一条弧：u 起点、v 终点、w 剩余容量、next 同起点的下一条弧。"""

    __slots__ = ("u", "v", "w", "next")

    def __init__(self, u: int, v: int, w: int, next: int) -> None:
        self.u = u
        self.v = v
        self.w = w
        self.next = next


class LinkList:
    """链式前向星：h[u] 指向 u 最近加入的弧，-1 表示链尾（defaultdict 模拟 memset -1）。"""

    edge_cnt: int
    e: list[Edge]
    h: defaultdict[int, int]

    def __init__(self) -> None:
        self.reset()

    def reset(self) -> None:
        """清空整张图（对应 C++ 的 edge_cnt = 0 + memset(h, -1)）。"""
        self.edge_cnt = 0
        self.e = []
        self.h = defaultdict(lambda: -1)

    def add(self, u: int, v: int, w: int = 0) -> None:
        """加弧 u -> v，O(1) 插到 u 的链表头；w 是容量。"""
        self.e.append(Edge(u, v, w, self.h[u]))
        self.h[u] = self.edge_cnt
        self.edge_cnt += 1

    def __getitem__(self, i: int) -> Edge:
        # 对应 C++ 的 operator[]，返回可改字段的对象，改 w 会直接改残留网络。
        return self.e[i]


e = LinkList()  # 模块级图，与 C++ 的全局 e 对应；同一时刻只服务一个 Dinic 实例


class Dinic:
    """level 是 BFS 分层，cur 是当前弧优化的游标。"""

    level: IntList
    cur: IntList

    def __init__(self, n: int = 0) -> None:
        self.init(n)

    def init(self, n: int) -> None:
        """重置图并分配 level/cur；n 至少要覆盖最大点编号。"""
        e.reset()
        self.n = n
        self.level = [-1] * (n + 5)
        self.cur = [-1] * (n + 5)

    def add_edge(self, u: int, v: int, cap: int) -> None:
        """加容量 cap 的边，同时补一条容量 0 的反向边（编号与正向边异或 1 配对）。"""
        e.add(u, v, cap)
        e.add(v, u, 0)

    def bfs(self, s: int, t: int) -> bool:
        """按残留容量分层；返回 t 是否可达。"""
        self.level = [-1] * (self.n + 5)
        self.level[s] = 0
        q = deque([s])
        while q:
            u = q.popleft()
            i = e.h[u]
            while i != -1:
                edge = e[i]
                v = edge.v
                if edge.w > 0 and self.level[v] < 0:
                    self.level[v] = self.level[u] + 1
                    q.append(v)
                i = edge.next
        return self.level[t] >= 0

    def dfs(self, u: int, t: int, pre_flow: int) -> int:
        """从 u 向 t 增广，可推送的上限是 pre_flow；返回实际推送量。"""
        if u == t or pre_flow == 0:
            return pre_flow
        flow = 0
        while self.cur[u] != -1:
            cid = self.cur[u]
            edge = e[cid]
            to = edge.v
            cap = edge.w
            nxt = edge.next
            # 只走层次图上相邻的一层，且必须有残余容量。
            if self.level[u] + 1 != self.level[to] or cap <= 0:
                self.cur[u] = nxt
                continue

            tr = self.dfs(to, t, min(pre_flow, cap))
            e[cid].w -= tr  # 正向边容量减少
            e[cid ^ 1].w += tr  # 反向边容量增加，异或 1 取到配对边
            flow += tr
            pre_flow -= tr
            if pre_flow == 0:
                break
            self.cur[u] = nxt

        # 炸点优化：从 u 已经流不出去了，本次分层内不再进入 u。
        if flow == 0:
            self.level[u] = -1
        return flow

    def max_flow(self, s: int, t: int) -> int:
        flow = 0
        while self.bfs(s, t):
            # 当前弧优化重置：每个点的游标回到它第一条出边。
            for i in range(self.n + 1):
                self.cur[i] = e.h[i]
            flow += self.dfs(s, t, LLONG_MAX)  # 多路增广
        return flow
