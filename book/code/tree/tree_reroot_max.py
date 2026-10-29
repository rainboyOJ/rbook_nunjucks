# 依赖模块级全局邻接表 tree：使用前先 tree = [[] for _ in range(n + 1)]，
# 再加无权边 tree[u].append(v) 和 tree[v].append(u)。节点编号 1..n。
# dfs 递归深度最坏 n（1e5+），使用前需 sys.setrecursionlimit(1 << 20)。

type Graph = list[list[int]]

tree: Graph = []


class TreeRerootMax:
    """换根 DP（最值型）：每个点到其它点的最远距离（偏心距），最小的点就是树的中心。"""

    n: int
    root: int
    down: list[int]
    up: list[int]
    ans: list[int]

    def __init__(self, n: int) -> None:
        self.n = n
        self.root = 1
        self.down = [0] * (n + 1)
        self.up = [0] * (n + 1)
        self.ans = [0] * (n + 1)

    def solve(self, start: int = 1) -> list[int]:
        """返回 ans 列表：ans[u] 就是 u 的偏心距。"""
        self.root = start
        self.down = [0] * (self.n + 1)
        self.up = [0] * (self.n + 1)
        self.dfs_down(self.root, 0)
        self.dfs_up(self.root, 0)

        for u in range(1, self.n + 1):
            self.ans[u] = max(self.down[u], self.up[u])
        return self.ans

    def dfs_down(self, u: int, parent: int) -> None:
        """第一次 DFS（后序）：down[u] 是 u 子树内离 u 最远的距离。"""
        self.down[u] = 0
        for v in tree[u]:
            if v == parent:
                continue
            self.dfs_down(v, u)
            if self.down[v] + 1 > self.down[u]:
                self.down[u] = self.down[v] + 1

    def dfs_up(self, u: int, parent: int) -> None:
        """第二次 DFS（前序）：算出每个儿子往上走的最远距离 up[v]。"""
        best1 = -1  # 子树方向最长距离
        best2 = -1  # 来自不同儿子的次长距离

        for v in tree[u]:
            if v == parent:
                continue
            candidate = self.down[v] + 1
            if candidate > best1:
                best2 = best1
                best1 = candidate
            elif candidate > best2:
                best2 = candidate

        for v in tree[u]:
            if v == parent:
                continue

            # 从 u 出发、不进入 v 子树的最远距离：v 提供最长时改用次长
            outside = best2 if self.down[v] + 1 == best1 else best1
            if self.up[u] > outside:
                outside = self.up[u]
            if outside < 0:
                outside = 0

            self.up[v] = outside + 1

        for v in tree[u]:
            if v == parent:
                continue
            self.dfs_up(v, u)
