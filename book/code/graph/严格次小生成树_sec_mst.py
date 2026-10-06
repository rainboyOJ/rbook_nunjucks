# 严格次小生成树：点编号 1..n，add_edge(u, v, w, id) 加边，solve() 返回严格次小生成树权值，
# 无生成树或无严格次小生成树时返回 -1（id 仅供调用方标识边，算法内部不用）。做法：先求 MST，
# 再倍增维护树上路径的「最大、严格次大」边权并枚举非树边替换；边权须 > NEG，Python int 不溢出。

type Pair = tuple[int, int]  # (路径上最大边权, 路径上严格次大边权)，缺失用 NEG 表示

NEG = -(1 << 60)  # 「不存在」哨兵，须小于任何合法边权
INF = 1 << 62  # 答案上界哨兵，须大于任何合法答案


class Edge:
    """无向边；used 由 solve 内部维护，标记该边是否被选入 MST。"""

    __slots__ = ("u", "v", "w", "id", "used")

    def __init__(self, u: int, v: int, w: int, id: int) -> None:
        self.u = u
        self.v = v
        self.w = w
        self.id = id
        self.used = False


class DSU:
    """parent[x] 为 x 的父节点；只做路径压缩，不按秩合并，与 C++ 版行为一致。"""

    parent: list[int]

    def __init__(self, n: int = 0) -> None:
        self.init(n)

    def init(self, n: int) -> None:
        self.parent = list(range(n + 1))

    def find(self, x: int) -> int:
        # 不按秩合并时链最坏可达 n，递归会爆栈，故两趟迭代做路径压缩。
        root = x
        while self.parent[root] != root:
            root = self.parent[root]
        while self.parent[x] != root:
            self.parent[x], x = root, self.parent[x]
        return root

    def merge(self, a: int, b: int) -> bool:
        a = self.find(a)
        b = self.find(b)
        if a == b:
            return False
        self.parent[a] = b
        return True


class StrictSecondMST:
    """倍增表 up[k][u] 为 u 的 2^k 级祖先，mx1/mx2 为对应跳段的严格最大/次大边权。"""

    n: int
    lg: int
    edges: list[Edge]
    tree: list[list[tuple[int, int]]]  # tree[u] 存 (邻点, 边权)
    depth: list[int]
    up: list[list[int]]
    mx1: list[list[int]]
    mx2: list[list[int]]

    def __init__(self, n: int) -> None:
        self.n = n
        self.lg = 1
        while (1 << self.lg) <= n:
            self.lg += 1  # lg 是最小的使 2^lg > n 的幂次，n=1 时 lg=1
        self.edges = []
        self.tree = [[] for _ in range(n + 1)]
        self.depth = [0] * (n + 1)
        # up 的每一行长度 n+1，第 0 列（点 0）恒为 0，充当「祖先不存在」的黑洞。
        self.up = [[0] * (n + 1) for _ in range(self.lg)]
        self.mx1 = [[NEG] * (n + 1) for _ in range(self.lg)]
        self.mx2 = [[NEG] * (n + 1) for _ in range(self.lg)]

    def add_edge(self, u: int, v: int, w: int, id: int) -> None:
        self.edges.append(Edge(u, v, w, id))

    def add_value(self, x: int, a: int, b: int) -> Pair:
        """把 x 并入 (a, b) 这对「最大、严格次大」，返回更新后的 (a, b)。

        C++ 用引用原地改 a、b，Python 没有引用参数，改为返回新元组，语义一致。
        """
        if x == NEG:
            return a, b
        if x > a:
            b = a  # x 取代 a 成为最大，原最大降为次大
            a = x
        elif x < a and x > b:
            b = x  # x 落在 a 与 b 之间，成为新的严格次大
        return a, b

    def merge_pair(self, x: Pair, y: Pair) -> Pair:
        """合并两段路径的 (最大, 严格次大)；重复值只保留一份。"""
        a, b = NEG, NEG
        a, b = self.add_value(x[0], a, b)
        a, b = self.add_value(x[1], a, b)
        a, b = self.add_value(y[0], a, b)
        a, b = self.add_value(y[1], a, b)
        return a, b

    def dfs(self, root: int, fa: int) -> None:
        # 树链深度最坏 n，递归会爆 Python 栈，改显式栈；树无环，靠父节点挡回头路即可。
        stack = [(root, fa)]
        while stack:
            u, p = stack.pop()
            for v, w in self.tree[u]:
                if v == p:
                    continue
                self.depth[v] = self.depth[u] + 1
                self.up[0][v] = u
                self.mx1[0][v] = w
                stack.append((v, u))

    def build_lca(self) -> None:
        self.depth[1] = 1  # 根深度从 1 起，与 C++ 一致
        self.dfs(1, 0)

        for k in range(1, self.lg):
            for u in range(1, self.n + 1):
                mid = self.up[k - 1][u]
                self.up[k][u] = self.up[k - 1][mid]
                self.mx1[k][u], self.mx2[k][u] = self.merge_pair(
                    (self.mx1[k - 1][u], self.mx2[k - 1][u]),
                    (self.mx1[k - 1][mid], self.mx2[k - 1][mid]),
                )

    def path_max_two(self, a: int, b: int) -> Pair:
        """返回树上 a-b 路径的 (最大边权, 严格次大边权)，缺失分量用 NEG。"""
        ans: Pair = (NEG, NEG)

        if self.depth[a] < self.depth[b]:
            a, b = b, a
        diff = self.depth[a] - self.depth[b]
        for k in range(self.lg):
            if (diff >> k) & 1:  # 二进制第 k 位为 1 就跳 2^k
                ans = self.merge_pair(ans, (self.mx1[k][a], self.mx2[k][a]))
                a = self.up[k][a]

        if a == b:
            return ans

        for k in range(self.lg - 1, -1, -1):
            if self.up[k][a] != self.up[k][b]:
                ans = self.merge_pair(ans, (self.mx1[k][a], self.mx2[k][a]))
                ans = self.merge_pair(ans, (self.mx1[k][b], self.mx2[k][b]))
                a = self.up[k][a]
                b = self.up[k][b]

        # 此时 a、b 是 LCA 的两个孩子，再补上各自到 LCA 的一条边。
        ans = self.merge_pair(ans, (self.mx1[0][a], self.mx2[0][a]))
        ans = self.merge_pair(ans, (self.mx1[0][b], self.mx2[0][b]))
        return ans

    def solve(self) -> int:
        """返回严格次小生成树权值；无生成树或无严格次小生成树时返回 -1。"""
        self.edges.sort(key=lambda e: e.w)  # 只按 w 排序，对应 Edge::operator<
        dsu = DSU(self.n)

        mst = 0
        cnt = 0
        for e in self.edges:
            if not dsu.merge(e.u, e.v):
                continue
            e.used = True
            mst += e.w
            cnt += 1
            self.tree[e.u].append((e.v, e.w))
            self.tree[e.v].append((e.u, e.w))

        if cnt != self.n - 1:
            return -1  # 图不连通，没有生成树

        self.build_lca()

        ans = INF
        for e in self.edges:
            if e.used:
                continue
            largest, second_largest = self.path_max_two(e.u, e.v)

            # 要严格变小，只能删严格小于 e.w 的边：优先删最大，最大不满足再退而求次大。
            removed = NEG
            if largest < e.w:
                removed = largest
            elif second_largest < e.w:
                removed = second_largest

            if removed != NEG:
                ans = min(ans, mst + e.w - removed)

        return -1 if ans == INF else ans
