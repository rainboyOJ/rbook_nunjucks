# 树的存储：父亲表示法（parent / 双亲表示法）
# 每个点只记录自己的父亲 fa[u]，根的父亲记为 0。
# 信息方向：向上走一步 O(1)；向下找一个孩子要扫描全部点 O(n)。
# 适用：自底向上递推、按父亲跳祖先、只关心"向上"信息的问题。
# 如何使用：
# 1. pt = ParentTree(n, root)
# 2. pt.set_fa(v, u)              # u 是 v 的父亲，即 fa[v] = u
# 3. pt.for_each_ancestor(v, f)   # 从 v 向上依次访问所有真祖先
# 4. pt.depth(v)                  # v 的深度，根的深度为 0

from collections.abc import Callable


class ParentTree:
    """fa[v] 是 v 的父亲；fa[root] = 0，根没有父亲。"""

    n: int
    root: int
    fa: list[int]

    def __init__(self, n: int, root: int = 1) -> None:
        self.n = n
        self.root = root
        self.fa = [0] * (n + 1)  # 下标 1..n

    def set_fa(self, v: int, f: int) -> None:
        """把 v 的父亲设成 f；注意参数顺序和 fa[v] = f 一致。"""
        self.fa[v] = f

    def is_root(self, u: int) -> bool:
        return self.fa[u] == 0

    def for_each_ancestor(self, v: int, func: Callable[[int], None]) -> None:
        """从 v 向上依次访问所有真祖先（不含 v 自己），一路走到根。"""
        u = self.fa[v]
        while u != 0:
            func(u)
            u = self.fa[u]

    def depth(self, v: int) -> int:
        """v 的深度：从 v 走到根要经过多少条边（树中路径唯一，步数确定）。"""
        d = 0
        u = self.fa[v]
        while u != 0:
            d += 1
            u = self.fa[u]
        return d
