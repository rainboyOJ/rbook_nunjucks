# 树的存储：孩子表示法
# 每个点记录自己的孩子列表 ch[u]，顺手记下父亲 fa[u]，向上向下就都 O(1) 了。
# 枚举 u 的全部孩子只要 O(孩子数)；空间 O(n)，所有列表一共只装 n-1 个孩子。
# 适用：树形 DP、DFS/BFS 遍历、子树统计等"向下走"的问题。
# 如何使用：
# 1. ct = ChildTree(n, root)
# 2. ct.add_child(u, v)       # u 是 v 的父亲
# 3. ct.for_each_child(u, f)  # 遍历 u 的所有孩子
# 4. ct.fa[u]                 # 向上找 u 的父亲

from collections.abc import Callable


class ChildTree:
    """ch[u] 是 u 的孩子列表（按加入顺序）；fa[u] 是 u 的父亲，fa[root] = 0。"""

    n: int
    root: int
    ch: list[list[int]]
    fa: list[int]

    def __init__(self, n: int, root: int = 1) -> None:
        self.n = n
        self.root = root
        self.ch = [[] for _ in range(n + 1)]  # 下标 1..n
        self.fa = [0] * (n + 1)

    def add_child(self, u: int, v: int) -> None:
        """u 是 v 的父亲：把 v 挂到 u 的孩子列表末尾。"""
        self.ch[u].append(v)
        self.fa[v] = u

    def for_each_child(self, u: int, func: Callable[[int], None]) -> None:
        """遍历 u 的所有孩子。"""
        for v in self.ch[u]:
            func(v)
