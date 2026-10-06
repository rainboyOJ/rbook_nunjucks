# 依赖模块级全局邻接表 tree：使用前先 tree = [[] for _ in range(n + 1)]，
# 再加无权边 tree[u].append(v) 和 tree[v].append(u)。节点编号 1..n。
# 重心：删除该点后剩下的每个连通块大小都不超过 n/2。
# dfs 递归深度最坏 n（1e5+），使用前需 sys.setrecursionlimit(1 << 20)。


type Graph = list[list[int]]

tree: Graph = []


class TreeCentroid:
    """先求子树大小，再取 B(u) = 删 u 后最大连通块最小的点，可能不止一个。"""

    n: int
    sz: list[int]
    ans: list[int]
    best: int

    def __init__(self, n: int) -> None:
        self.n = n
        self.best = n
        self.sz = [0] * (n + 1)
        self.ans = []

    def find_centroids(self, root: int = 1) -> list[int]:
        """返回所有重心，编号升序。"""
        self.best = self.n
        self.ans = []
        self.dfs(root, 0)
        self.ans.sort()
        return self.ans

    def dfs(self, u: int, parent: int) -> None:
        self.sz[u] = 1
        mx = 0  # B(u)：先看各儿子子树

        for v in tree[u]:
            if v == parent:
                continue
            self.dfs(v, u)
            self.sz[u] += self.sz[v]
            if self.sz[v] > mx:
                mx = self.sz[v]

        # 父亲方向也是一块：整棵树减去 u 的子树
        if self.n - self.sz[u] > mx:
            mx = self.n - self.sz[u]

        # 记录 B(u) 最小的点（可能不止一个）
        if mx < self.best:
            self.best = mx
            self.ans = [u]
        elif mx == self.best:
            self.ans.append(u)
