# Edmonds-Karp 最大流：每条有向边必须成对加——先 e.add(u, v, cap)，再 e.add(v, u, 0)。
# 成对编号后 i^1 就是反向边，回溯时 O(1) 找到它；漏加反向边会让结果偏小。
# 调用契约：e.reset(); 按上面方式加边; s=源点; t=汇点; ans=ek()。Python int 任意精度，流量不溢出。

from collections import defaultdict, deque

INF = 10**18  # C++ 的 1e18 用 ll 存；Python int 任意精度，比较大小同样安全

n = 0  # 点数（仅记录，算法本身只用到 e 与 s/t）
m = 0  # 边数（仅记录）
s = 0  # 源点
t = 0  # 汇点


class Edge:
    """残留网络中的一条边：u 起点、v 终点、w 剩余容量、next 下一条出边编号。"""

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

    def __call__(self, u: int) -> int:
        # 对应 C++ 的 operator()(u)：返回点 u 的第一条出边编号。
        return self.h[u]


e = LinkList()

pre: defaultdict[int, int] = defaultdict(lambda: -1)  # pre[v]：通向 v 的那条边编号，-1 未访问
flow: dict[int, int] = {}  # flow[v]：s 到 v 这条路径的瓶颈容量


def bfs() -> bool:
    """在残留网络上 BFS 找一条 s->t 的增广路；找到就填好 pre/flow 并返回 True。"""
    pre.clear()
    q: deque[int] = deque([s])
    flow[s] = INF  # 源点不设上限
    pre[s] = 0  # 用 0 表示源点已访问（合法边编号非负，不会和 -1 冲突）
    while q:
        u = q.popleft()
        if u == t:
            return True  # 搜到汇点即可提前结束，路径信息已足够
        i = e.h[u]
        while i != -1:
            edge = e[i]
            if pre[edge.v] == -1 and edge.w > 0:
                pre[edge.v] = i
                flow[edge.v] = min(flow[u], edge.w)
                q.append(edge.v)
            i = edge.next
    return False


def ek() -> int:
    """返回 s->t 的最大流；s == t 是非法输入（题目约束 s != t），防御性返回 0。"""
    # s == t 时 bfs() 会立即“找到”一条空增广路，瓶颈是 INF 且增广不修改任何边，
    # 主循环将永不终止，所以必须在这里拦截。
    if s == t:
        return 0
    max_flow = 0
    while bfs():
        increment = flow[t]
        max_flow += increment
        v = t
        while v != s:
            i = pre[v]
            e[i].w -= increment  # 正向边剩余容量减少
            e[i ^ 1].w += increment  # 反向边容量增加，允许反悔
            v = e[i ^ 1].v  # 反向边终点就是正向边起点
    return max_flow
