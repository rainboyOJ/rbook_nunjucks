# Tarjan 求无向图割边（桥）。依赖模块级链式前向星全局 e（本文件内联了最小实现，完整版见 linklist.py）：
#   e.reset(); e.add2(u, v)   # 必须成对加边，正向边编号为偶数，i ^ 1 是它的反向边
#   t = TarjanBridge(); t.set(n); t.solve(); bridges = t.get_bridges()
# 割边条件 low[v] > dfn[u]（不能取等号）；用 in_edge ^ 1 屏蔽来路，才能正确处理重边。
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


class TarjanBridge:
    """dfn/low 为时间戳；is_bridge 按边编号标记，一对反向边同时置位。"""

    def __init__(self) -> None:
        self.n = 0
        self.timer = 0
        self.dfn: list[int] = []
        self.low: list[int] = []
        self.is_bridge: list[bool] = []

    def set(self, _n: int) -> None:
        """按当前边数重新分配；set 与 add2 的先后顺序都可以，solve 里会补足长度。"""
        self.n = _n
        self.timer = 0
        self.dfn = [0] * (_n + 1)
        self.low = [0] * (_n + 1)
        self.is_bridge = [False] * e.edge_cnt

    def dfs(self, u: int, in_edge: int) -> None:
        """in_edge 是进入 u 的那条边的编号（根传 -1），用于屏蔽来路而不是屏蔽父节点。"""
        self.timer += 1
        self.dfn[u] = self.low[u] = self.timer

        i = e.h[u]
        while i != -1:
            v = e[i].v
            # in_edge ^ 1 是来路的反向边；in_edge = -1 时 -1 ^ 1 = -2，不会与合法边号撞车。
            if i != (in_edge ^ 1):
                if self.dfn[v] == 0:  # 树枝边
                    self.dfs(v, i)
                    if self.low[v] < self.low[u]:
                        self.low[u] = self.low[v]
                    # 严格大于：v 子树连 u 都回不到，u-v 才是唯一的通道。
                    if self.low[v] > self.dfn[u]:
                        self.is_bridge[i] = True
                        self.is_bridge[i ^ 1] = True
                elif self.dfn[v] < self.dfn[u]:  # 回边（只取祖先，忽略已处理过的子节点）
                    if self.dfn[v] < self.low[u]:
                        self.low[u] = self.dfn[v]
            i = e[i].next

    def solve(self) -> None:
        # 若 set 在 add2 之前调用，这里按当前边数补足标记数组（新增边默认不是桥）。
        if len(self.is_bridge) < e.edge_cnt:
            self.is_bridge.extend([False] * (e.edge_cnt - len(self.is_bridge)))
        for i in range(1, self.n + 1):
            if self.dfn[i] == 0:
                self.dfs(i, -1)  # 根没有进入边，传 -1

    def get_bridges(self) -> list[tuple[int, int]]:
        """以 (u, v) 形式返回所有桥；只扫偶数号边，避免同一对重复添加。"""
        ans: list[tuple[int, int]] = []
        for i in range(0, e.edge_cnt, 2):
            if self.is_bridge[i]:
                ans.append((e[i ^ 1].v, e[i].v))
        return ans
