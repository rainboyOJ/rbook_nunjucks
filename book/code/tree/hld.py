# 树链剖分（HLD）+ 带懒标记的区间加、区间和线段树，全部对 mod 取模。
# 节点编号 1..n，dfn 是树剖序（1 下标）：重链在 dfn 上连续，子树也连续，于是路径/子树都变成区间操作。
# 调用契约：hld = HeavyLightDecomposition(n, root, mod)；填 hld.value[1..n]（建议先 % mod）；
#           add_edge 建树；hld.build()；之后用 path_add / path_sum / subtree_add / subtree_sum。
# SegmentTree 内部 sum/lazy 按 n * 4 + 5 开；mod 必须为正整数。
# C++ 的 % 对负数会给负结果，Python 的 % 恒非负；模意义下同余，最终答案一致。
# dfs_size / dfs_decompose 递归深度最坏 n（1e5+），使用前需 sys.setrecursionlimit(1 << 20)。


type Seq = list[int]


class SegmentTree:
    """区间加、区间和，sum[u] / lazy[u] 分别存节点 u 的和与待下传的加量。"""

    sum: Seq
    lazy: Seq
    mod: int

    def __init__(self, n: int = 0, mod_value: int = 1) -> None:
        self.init(n, mod_value)

    def init(self, n: int, mod_value: int) -> None:
        self.mod = mod_value
        self.sum = [0] * (n * 4 + 5)
        self.lazy = [0] * (n * 4 + 5)

    def apply(self, u: int, l: int, r: int, value: int) -> None:
        value %= self.mod
        # 区间长度是 r - l + 1；先取模再加，防止数值无限增长。
        self.sum[u] = (self.sum[u] + value * (r - l + 1)) % self.mod
        self.lazy[u] = (self.lazy[u] + value) % self.mod

    def pushdown(self, u: int, l: int, r: int) -> None:
        # 叶节点没有儿子可下传；lazy 为 0 时无事可做。
        if self.lazy[u] == 0 or l == r:
            return
        mid = (l + r) >> 1  # 除以 2 向下取整
        self.apply(u << 1, l, mid, self.lazy[u])  # 左儿子编号 2u
        self.apply(u << 1 | 1, mid + 1, r, self.lazy[u])  # 右儿子编号 2u+1
        self.lazy[u] = 0

    def build(self, u: int, l: int, r: int, base: Seq) -> None:
        if l == r:
            self.sum[u] = base[l] % self.mod
            return
        mid = (l + r) >> 1
        self.build(u << 1, l, mid, base)
        self.build(u << 1 | 1, mid + 1, r, base)
        self.sum[u] = (self.sum[u << 1] + self.sum[u << 1 | 1]) % self.mod

    def range_add(self, ql: int, qr: int, value: int, u: int, l: int, r: int) -> None:
        if ql <= l and r <= qr:
            self.apply(u, l, r, value)
            return
        self.pushdown(u, l, r)
        mid = (l + r) >> 1
        if ql <= mid:
            self.range_add(ql, qr, value, u << 1, l, mid)
        if qr > mid:
            self.range_add(ql, qr, value, u << 1 | 1, mid + 1, r)
        self.sum[u] = (self.sum[u << 1] + self.sum[u << 1 | 1]) % self.mod

    def range_sum(self, ql: int, qr: int, u: int, l: int, r: int) -> int:
        if ql <= l and r <= qr:
            return self.sum[u]
        self.pushdown(u, l, r)
        mid = (l + r) >> 1
        answer = 0
        if ql <= mid:
            answer += self.range_sum(ql, qr, u << 1, l, mid)
        if qr > mid:
            answer += self.range_sum(ql, qr, u << 1 | 1, mid + 1, r)
        return answer % self.mod


class HeavyLightDecomposition:
    """两遍 DFS 定重儿子和 dfn，再用线段树维护路径/子树操作。"""

    n: int
    root: int
    mod: int
    timer: int
    graph: list[list[int]]
    parent: Seq
    depth: Seq
    subtree_size: Seq
    heavy_son: Seq
    top: Seq
    dfn: Seq
    node_at: Seq
    value: Seq
    ordered_value: Seq
    seg: SegmentTree

    def __init__(self, n: int, root: int, mod: int) -> None:
        self.n = n
        self.root = root
        self.mod = mod
        self.timer = 0
        self.graph = [[] for _ in range(n + 1)]
        self.parent = [0] * (n + 1)
        self.depth = [0] * (n + 1)
        self.subtree_size = [0] * (n + 1)
        # heavy_son 用 0 表示没有重儿子，subtree_size[0] = 0 让比较天然成立。
        self.heavy_son = [0] * (n + 1)
        self.top = [0] * (n + 1)
        self.dfn = [0] * (n + 1)
        self.node_at = [0] * (n + 1)
        self.value = [0] * (n + 1)
        self.ordered_value = [0] * (n + 1)
        self.seg = SegmentTree(n, mod)

    def add_edge(self, u: int, v: int) -> None:
        self.graph[u].append(v)
        self.graph[v].append(u)

    def dfs_size(self, u: int, father: int) -> None:
        self.parent[u] = father
        self.depth[u] = self.depth[father] + 1
        self.subtree_size[u] = 1
        self.heavy_son[u] = 0

        for v in self.graph[u]:
            if v == father:
                continue
            self.dfs_size(v, u)
            self.subtree_size[u] += self.subtree_size[v]
            if self.heavy_son[u] == 0 or self.subtree_size[v] > self.subtree_size[self.heavy_son[u]]:
                self.heavy_son[u] = v

    def dfs_decompose(self, u: int, chain_top: int) -> None:
        self.top[u] = chain_top
        self.timer += 1
        self.dfn[u] = self.timer
        self.node_at[self.timer] = u
        self.ordered_value[self.timer] = self.value[u]

        # 先走重儿子，保证重链在 dfn 上连续。
        if self.heavy_son[u] != 0:
            self.dfs_decompose(self.heavy_son[u], chain_top)

        for v in self.graph[u]:
            if v == self.parent[u] or v == self.heavy_son[u]:
                continue
            self.dfs_decompose(v, v)

    def build(self) -> None:
        self.dfs_size(self.root, 0)
        self.dfs_decompose(self.root, self.root)
        self.seg.build(1, 1, self.n, self.ordered_value)

    def path_add(self, u: int, v: int, delta: int) -> None:
        while self.top[u] != self.top[v]:
            # 链顶浅的那条整段往上跳，跳完接上链顶的父亲。
            if self.depth[self.top[u]] < self.depth[self.top[v]]:
                u, v = v, u
            self.seg.range_add(self.dfn[self.top[u]], self.dfn[u], delta, 1, 1, self.n)
            u = self.parent[self.top[u]]
        # 同一条链上：浅的是 LCA，区间就是 [dfn[u], dfn[v]]。
        if self.depth[u] > self.depth[v]:
            u, v = v, u
        self.seg.range_add(self.dfn[u], self.dfn[v], delta, 1, 1, self.n)

    def path_sum(self, u: int, v: int) -> int:
        answer = 0
        while self.top[u] != self.top[v]:
            if self.depth[self.top[u]] < self.depth[self.top[v]]:
                u, v = v, u
            answer += self.seg.range_sum(self.dfn[self.top[u]], self.dfn[u], 1, 1, self.n)
            answer %= self.mod
            u = self.parent[self.top[u]]
        if self.depth[u] > self.depth[v]:
            u, v = v, u
        answer += self.seg.range_sum(self.dfn[u], self.dfn[v], 1, 1, self.n)
        return answer % self.mod

    def subtree_add(self, u: int, delta: int) -> None:
        self.seg.range_add(self.dfn[u], self.dfn[u] + self.subtree_size[u] - 1, delta, 1, 1, self.n)

    def subtree_sum(self, u: int) -> int:
        return self.seg.range_sum(self.dfn[u], self.dfn[u] + self.subtree_size[u] - 1, 1, 1, self.n)
