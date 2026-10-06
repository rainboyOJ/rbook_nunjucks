# 最小费用最大流（SPFA 连续最短路）：点编号 0..n，add_edge 自动补反向边（容量 0、费用 -cost）。
# e 是模块级链式前向星，对应 C++ 的全局 linkList；init(n) 会重置整张图，须在加边前调用。
# dis/flow 用 INF_INT = 0x3f3f3f3f 当无穷；C++ 里 dis[u] + cost 可能溢出 int，Python int 任意精度。
# 图中不能有负权回路，否则 SPFA 不收敛；先 init(n) 再 add_edge，最后 solve(s, t)。

from collections import defaultdict, deque

INF_INT = 0x3F3F3F3F  # 对应 C++ 的 INF_INT，SPFA 松弛的初始距离与流量上界

type IntList = list[int]  # 与点一一对应的定长数组，下标 0..n+1


class Edge:
    """一条弧：u 起点、v 终点、w 剩余容量、c 单位费用、next 同起点的下一条弧。"""

    __slots__ = ("u", "v", "w", "c", "next")

    def __init__(self, u: int, v: int, w: int, c: int, next: int) -> None:
        self.u = u
        self.v = v
        self.w = w
        self.c = c
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

    def add(self, u: int, v: int, w: int = 0, c: int = 0) -> None:
        """加弧 u -> v，O(1) 插到 u 的链表头；w 是容量、c 是单位费用。"""
        self.e.append(Edge(u, v, w, c, self.h[u]))
        self.h[u] = self.edge_cnt
        self.edge_cnt += 1

    def __getitem__(self, i: int) -> Edge:
        # 对应 C++ 的 operator[]，返回可改字段的对象，改 w 会直接改残留网络。
        return self.e[i]


e = LinkList()  # 模块级图，与 C++ 的全局 e 对应；同一时刻只服务一个 MCMF 实例


class MCMF:
    """结果在 max_flow / min_cost 两个成员里，solve 结束后读取。"""

    dis: IntList
    flow: IntList
    pre: IntList
    last: IntList
    vis: list[bool]

    def __init__(self, n: int = 0) -> None:
        self.init(n)

    def init(self, n: int) -> None:
        """重置图并把数组开到能覆盖 0..n+1；必须覆盖所有出现的点编号。"""
        e.reset()
        self.n = n
        self.dis = [INF_INT] * (n + 2)
        self.flow = [INF_INT] * (n + 2)
        self.pre = [-1] * (n + 2)
        self.last = [-1] * (n + 2)
        self.vis = [False] * (n + 2)

    def add_edge(self, u: int, v: int, cap: int, cost: int) -> None:
        """加容量 cap、单位费用 cost 的边，同时补一条容量 0、费用 -cost 的反向边。"""
        e.add(u, v, cap, cost)
        e.add(v, u, 0, -cost)

    def spfa(self, s: int, t: int) -> bool:
        """SPFA 求单位费用最小的增广路；返回 t 是否可达，路径记在 pre/last 里。"""
        for i in range(self.n + 2):
            self.dis[i] = INF_INT
            self.vis[i] = False
            self.flow[i] = INF_INT

        q = deque([s])
        self.dis[s] = 0
        self.vis[s] = True
        self.pre[t] = -1  # 标记汇点前驱，用于判断是否可达

        while q:
            u = q.popleft()
            self.vis[u] = False
            i = e.h[u]
            while i != -1:
                edge = e[i]
                v = edge.v
                cap = edge.w
                cost = edge.c
                # 有残余容量且路径费用更小：松弛，并记录前驱节点/边与最小流量。
                if cap > 0 and self.dis[v] > self.dis[u] + cost:
                    self.dis[v] = self.dis[u] + cost
                    self.pre[v] = u
                    self.last[v] = i
                    self.flow[v] = min(self.flow[u], cap)
                    if not self.vis[v]:
                        self.vis[v] = True
                        q.append(v)
                i = edge.next

        return self.pre[t] != -1

    def solve(self, s: int, t: int) -> None:
        """反复沿最小费用路增广，直到不可达；结果写入 max_flow 与 min_cost。"""
        self.max_flow = 0
        self.min_cost = 0
        while self.spfa(s, t):
            now = t
            f = self.flow[t]  # 本次增广的流量（路径上最小残余容量）
            self.max_flow += f
            self.min_cost += f * self.dis[t]
            while now != s:
                idx = self.last[now]
                e[idx].w -= f  # 正向边容量减少
                e[idx ^ 1].w += f  # 反向边容量增加，异或 1 取到配对边
                now = self.pre[now]
