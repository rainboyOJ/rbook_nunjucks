# 依赖模块级全局邻接表 tree：使用前先 tree = [[] for _ in range(n + 1)]，
# 再加无权边 tree[u].append(v) 和 tree[v].append(u)。节点编号 1..n，build 默认以 1 为根。
# up[u][j] 是 u 的 2^j 级祖先；越界时跳到 0（黑洞点），up[0][*] 恒为 0，因此无需额外判空。
# C++ 的 maxn = 1e6 + 5 是固定数组容量，Python 按 n 动态分配，故只保留 max_log。
# dfs 递归深度最坏 n（1e5+），使用前需 sys.setrecursionlimit(1 << 20)。


type Graph = list[list[int]]

tree: Graph = []


class BinaryLCA:
    """倍增法求 LCA 与树上距离。"""

    max_log: int = 20  # 支持最多 2^20 = 1,048,576 个节点

    n: int
    depth: list[int]
    up: list[list[int]]

    def __init__(self, n: int = 0) -> None:
        self.n = n
        # up[0] 一整行保持 0，充当「跳过头」的哨兵，省掉查询里的边界判断。
        self.up = [[0] * BinaryLCA.max_log for _ in range(n + 1)]
        self.depth = [0] * (n + 1)

    def dfs(self, u: int, fa: int) -> None:
        self.up[u][0] = fa
        self.depth[u] = self.depth[fa] + 1
        for j in range(1, BinaryLCA.max_log):
            # 先跳 2^(j-1) 再跳 2^(j-1)，拼成 2^j；1 << (j - 1) 是步长。
            self.up[u][j] = self.up[self.up[u][j - 1]][j - 1]
        for v in tree[u]:
            if v == fa:
                continue
            self.dfs(v, u)

    def build(self, root: int = 1) -> None:
        self.depth[0] = 0
        self.dfs(root, 0)

    def kth_ancestor(self, u: int, k: int) -> int:
        """向上跳 k 步（k 的二进制每个 1 位跳一次）。"""
        for j in range(BinaryLCA.max_log):
            if k & (1 << j):
                u = self.up[u][j]
        return u

    def lca(self, a: int, b: int) -> int:
        if self.depth[a] < self.depth[b]:
            a, b = b, a  # 保证 a 是较深的节点

        a = self.kth_ancestor(a, self.depth[a] - self.depth[b])
        if a == b:
            return a

        # 从高位到低位跳：跳到 LCA 的正下方，父节点就是答案。
        for j in range(BinaryLCA.max_log - 1, -1, -1):
            if self.up[a][j] != self.up[b][j]:
                a = self.up[a][j]
                b = self.up[b][j]
        return self.up[a][0]

    def dist(self, a: int, b: int) -> int:
        c = self.lca(a, b)
        return self.depth[a] + self.depth[b] - 2 * self.depth[c]
