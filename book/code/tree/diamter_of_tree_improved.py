# 依赖模块级全局链式前向星 head / e / cnt（C++ 用固定 maxn 数组，这里按需扩容）：
#   add_edge(u, v, w)       # w 默认 1；无向，双向各插一条边
#   solver = TreeDiameter(n) # n 是节点数，数组按 n + 1 分配（C++ 是模板参数 N 的定长数组）
#   solver.two_dfs(1)       # 或 solver.get_diameter_bfs(1)
# head[u] = 0 表示没有边（C++ 用 0 当空指针），e 从下标 1 开始，e[0] 占位不用。
# C++ 的 dis 与边权都是 int，路径和超过 2^31 - 1 会溢出；Python int 任意精度不会，
# 因此大权值下结果可能与 C++ 不同。
# dfs 递归深度最坏 n（1e5+），使用前需 sys.setrecursionlimit(1 << 20)。

from collections import deque


class Edge:
    """链式前向星的一条边：to 是终点，next 是下一条边下标，w 是边权。"""

    __slots__ = ("to", "next", "w")

    def __init__(self, to: int = 0, next: int = 0, w: int = 0) -> None:
        self.to = to
        self.next = next
        self.w = w


head: list[int] = [0]
e: list[Edge] = [Edge()]
cnt: int = 0


def add_edge(u: int, v: int, w: int = 1) -> None:
    global cnt
    # head 动态扩容到至少 max(u, v) + 1；C++ 是固定 maxn，语义一致。
    need = max(u, v) + 1
    if len(head) < need:
        head.extend([0] * (need - len(head)))
    while len(e) <= cnt + 2:
        e.append(Edge())

    cnt += 1
    e[cnt] = Edge(v, head[u], w)
    head[u] = cnt
    cnt += 1
    e[cnt] = Edge(u, head[v], w)
    head[v] = cnt


class TreeDiameter:
    """两遍 DFS / 两遍 BFS 求直径，并记录路径；边权为 1 时 BFS 与 DFS 等价。"""

    n: int
    dis: list[int]
    parent: list[int]
    farthest: list[int]
    st: int
    ed: int

    def __init__(self, n: int) -> None:
        self.n = n
        self.dis = []
        self.parent = []
        self.farthest = []
        self.st = 0
        self.ed = 0

    def reset(self) -> None:
        # C++ 的 head 是定长 maxn 数组；Python 里 n = 1 时 add_edge 没被调用过，
        # 所以这里补一次扩容，保证 head[1..n] 可访问。
        if len(head) < self.n + 1:
            head.extend([0] * (self.n + 1 - len(head)))
        # C++ 的 fill(dis, dis + N, 0) / fill(parent, parent + N, -1)，N 是模板参数；
        # 这里按节点数 n 分配，farthest 也一并分配（dfs 会写满它）。
        size = self.n + 1
        self.dis = [0] * size
        self.parent = [-1] * size
        self.farthest = [0] * size

    def dfs(self, u: int, fa: int, max_dist: int, far_node: int) -> tuple[int, int, int]:
        """C++ 的 max_dist / far_node 是引用出参，Python 改为随返回值一起带回。"""
        self.dis[u] = 0
        self.farthest[u] = u

        i = head[u]
        while i != 0:
            v = e[i].to
            if v != fa:
                _, max_dist, far_node = self.dfs(v, u, max_dist, far_node)
                length = self.dis[v] + e[i].w
                if length > self.dis[u]:
                    self.dis[u] = length
                    self.farthest[u] = self.farthest[v]
                    self.parent[u] = v
            i = e[i].next

        if self.dis[u] > max_dist:
            max_dist = self.dis[u]
            far_node = self.farthest[u]
        return self.farthest[u], max_dist, far_node

    def two_dfs(self, start_node: int = 1) -> int:
        self.reset()

        # 第一遍：从起始点找最远点
        max_dist, far_node = 0, start_node
        _, max_dist, far_node = self.dfs(start_node, 0, max_dist, far_node)
        self.st = far_node

        # 第二遍：从 st 找直径
        self.reset()
        max_dist = 0
        _, max_dist, far_node = self.dfs(self.st, 0, max_dist, far_node)
        self.ed = far_node

        return max_dist

    def bfs(self, start: int) -> int:
        self.reset()
        q: deque[int] = deque([start])
        self.dis[start] = 0
        self.parent[start] = -1

        farthest_node = start
        while q:
            u = q.popleft()
            i = head[u]
            while i != 0:
                v = e[i].to
                # 树中唯一会重复访问的就是父亲，用 parent[u] 即可判重。
                if v != self.parent[u]:
                    self.dis[v] = self.dis[u] + e[i].w
                    self.parent[v] = u
                    q.append(v)
                    if self.dis[v] > self.dis[farthest_node]:
                        farthest_node = v
                i = e[i].next
        return farthest_node

    def get_diameter_bfs(self, start_node: int = 1) -> int:
        self.st = self.bfs(start_node)
        self.ed = self.bfs(self.st)
        return self.dis[self.ed]

    def get_diameter_path(self) -> list[int]:
        path: list[int] = []
        u = self.ed
        while u != -1:
            path.append(u)
            u = self.parent[u]
        path.reverse()
        return path