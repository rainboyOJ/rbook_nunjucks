# 依赖模块级全局图 head / e / cnt / n（C++ 用固定 maxn 数组，这里由 init_graph 一次分配）：
#   init_graph(n)          # 等价 memset(head, 0) + cnt = 0，并按 n 分配 head 与 2n 条边
#   add_edge(u, v, w)      # w 默认 1；无向，双向各插一条边
#   solver = TreeDiameterAdvanced()
#   solver.get_diameter_dfs(1) / get_diameter_bfs(1) / get_diameter_dijkstra(1) / get_diameter(1)
# head[u] = 0 表示没有边（C++ 用 0 当空指针），e 从下标 1 开始，e[0] 占位不用。
# C++ 的 INF = 1e18 与 long long 在这里都是 Python int，任意精度，不会溢出。
# dfs 递归深度最坏 n（1e5+），使用前需 sys.setrecursionlimit(1 << 20)。

import heapq
from collections import deque


INF: int = 10**18  # 相当于 C++ 的 1e18，作为「不可达」哨兵

type Dist = list[int]


class Edge:
    """链式前向星的一条边：to 是终点，next 是下一条边下标，w 是边权。"""

    __slots__ = ("to", "next", "w")

    def __init__(self, to: int = 0, next: int = 0, w: int = 0) -> None:
        self.to = to
        self.next = next
        self.w = w


head: list[int] = []
e: list[Edge] = []
cnt: int = 0
n: int = 0


def init_graph(node_count: int) -> None:
    """布置全局图：head 全部置 0，预留 2n + 1 个边槽（n - 1 条无向边需要 2n - 2 条有向边）。"""
    global n, head, e, cnt
    n = node_count
    head = [0] * (n + 1)
    e = [Edge() for _ in range(2 * n + 1)]
    cnt = 0


def add_edge(u: int, v: int, w: int = 1) -> None:
    global cnt
    cnt += 1
    e[cnt] = Edge(v, head[u], w)
    head[u] = cnt
    cnt += 1
    e[cnt] = Edge(u, head[v], w)
    head[v] = cnt


class TreeDiameterAdvanced:
    """三种求直径的实现：DFS（加权树）、BFS（边权为 1）、Dijkstra（任意非负权）。"""

    dis: Dist
    parent: list[int]
    visited: list[bool]
    st: int
    ed: int
    diameter_len: int

    def __init__(self) -> None:
        self.dis = []
        self.parent = []
        self.visited = []
        self.st = 0
        self.ed = 0
        self.diameter_len = 0

    def reset(self) -> None:
        # C++ 的 fill(dis, dis + N, INF) / fill(parent, parent + N, -1) / fill(visited, ..., false)；
        # Python 按当前全局 n 重新分配，效果相同。
        self.dis = [INF] * (n + 1)
        self.parent = [-1] * (n + 1)
        self.visited = [False] * (n + 1)

    def dfs(self, u: int, fa: int) -> int:
        """加权树求最远点：dis[u] 是 u 往下走的最长距离，返回其端点。"""
        self.dis[u] = 0
        self.parent[u] = fa
        farthest_node = u

        i = head[u]
        while i != 0:
            v = e[i].to
            if v != fa:
                leaf = self.dfs(v, u)
                length = self.dis[v] + e[i].w
                if length > self.dis[u]:
                    self.dis[u] = length
                    farthest_node = leaf
            i = e[i].next
        return farthest_node

    def get_diameter_dfs(self, start_node: int = 1) -> int:
        self.reset()
        self.st = self.dfs(start_node, 0)  # 第一遍找端点 st

        self.reset()
        self.ed = self.dfs(self.st, 0)  # 第二遍从 st 找 ed
        # dis[u] 是以 u 为根的子树高度：根 st 的高度就是 st 到最远点 ed 的距离（即直径）。
        # 不能取 dis[self.ed]：ed 是叶子，叶子的子树高度恒为 0。
        self.diameter_len = self.dis[self.st]

        return self.diameter_len

    def bfs(self, start: int) -> int:
        self.reset()
        q: deque[int] = deque([start])
        self.dis[start] = 0
        self.parent[start] = -1

        farthest_node = start
        while q:
            u = q.popleft()
            self.visited[u] = True

            i = head[u]
            while i != 0:
                v = e[i].to
                if not self.visited[v]:
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
        self.diameter_len = self.dis[self.ed]
        return self.diameter_len

    def dijkstra(self, start: int) -> None:
        self.reset()
        self.dis[start] = 0
        # 小根堆按 (距离, 编号) 比较，与 C++ 的 greater<pair<ll, int>> 一致。
        pq: list[tuple[int, int]] = [(0, start)]

        while pq:
            d, u = heapq.heappop(pq)
            if d != self.dis[u]:
                continue  # 过期条目，跳过

            i = head[u]
            while i != 0:
                v = e[i].to
                if self.dis[v] > d + e[i].w:
                    self.dis[v] = d + e[i].w
                    self.parent[v] = u
                    heapq.heappush(pq, (self.dis[v], v))
                i = e[i].next

    def get_diameter_dijkstra(self, start_node: int = 1) -> int:
        self.dijkstra(start_node)
        # max_element 取第一个最大值，max(range(...)) 同样取首个最大下标。
        self.st = max(range(1, n + 1), key=lambda x: self.dis[x])

        self.dijkstra(self.st)
        self.ed = max(range(1, n + 1), key=lambda x: self.dis[x])
        self.diameter_len = self.dis[self.ed]

        return self.diameter_len

    def get_diameter_path(self) -> list[int]:
        path: list[int] = []
        u = self.ed
        while u != -1 and u != 0:
            path.append(u)
            u = self.parent[u]
        path.reverse()
        return path

    def get_min_dist_to_diameter(self) -> Dist:
        diameter_path = self.get_diameter_path()
        min_dist: Dist = [INF] * (n + 1)

        for u in diameter_path:
            self.reset()
            self.dijkstra(u)
            for i in range(1, n + 1):
                if self.dis[i] < min_dist[i]:
                    min_dist[i] = self.dis[i]
        return min_dist

    def get_diameter(self, start_node: int = 1) -> int:
        all_weight_one = True
        for i in range(1, cnt + 1):
            if e[i].w != 1:
                all_weight_one = False
                break

        # 100000：边权全 1 且规模不大时 DFS 最快，规模大时改用 BFS 防递归过深。
        if all_weight_one and n <= 100000:
            return self.get_diameter_dfs(start_node)
        elif all_weight_one:
            return self.get_diameter_bfs(start_node)
        else:
            return self.get_diameter_dijkstra(start_node)
