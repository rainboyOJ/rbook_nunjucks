# 依赖模块级全局邻接表 tree：使用前先 tree = [[] for _ in range(n + 1)]，
# 再加无权边 tree[u].append(v) 和 tree[v].append(u)。节点编号 1..n。
# 点权放在模块级 weight 列表里：weight[u] 是 u 的点权，无权版全部填 1。
# dfs 递归深度最坏 n（1e5+），使用前需 sys.setrecursionlimit(1 << 20)。

type Graph = list[list[int]]

tree: Graph = []
weight: list[int] = []


class TreeRerootSum:
    """换根 DP（求和型）：每个点作为根时的加权距离和 Σ weight[x] * dist(x, u)。"""

    n: int
    root: int
    total: int
    sub: list[int]
    ans: list[int]

    def __init__(self, n: int) -> None:
        self.n = n
        self.root = 1
        self.total = 0
        self.sub = [0] * (n + 1)
        self.ans = [0] * (n + 1)

    def solve(self, start: int = 1) -> list[int]:
        """返回 ans 列表：ans[u] 就是以 u 为根时的加权距离和。"""
        self.root = start
        self.total = sum(weight[1 : self.n + 1])
        self.ans = [0] * (self.n + 1)
        self.dfs_sum(self.root, 0, 0)
        self.dfs_reroot(self.root, 0)
        return self.ans

    def dfs_sum(self, u: int, parent: int, depth: int) -> None:
        """第一次 DFS：统计子树点权和，同时把根的答案按深度直接算出来。"""
        self.sub[u] = weight[u]
        self.ans[self.root] += weight[u] * depth

        for v in tree[u]:
            if v == parent:
                continue
            self.dfs_sum(v, u, depth + 1)
            self.sub[u] += self.sub[v]

    def dfs_reroot(self, u: int, parent: int) -> None:
        """第二次 DFS：根从 u 换到儿子 v，v 子树内少走一步、其余多走一步。"""
        for v in tree[u]:
            if v == parent:
                continue
            self.ans[v] = self.ans[u] + self.total - 2 * self.sub[v]
            self.dfs_reroot(v, u)
