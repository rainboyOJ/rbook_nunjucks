# Kruskal 最小生成树：点编号 1..n（parent 开 n+1，下标 0 是哨兵，无实际含义）。
# kruskal(n, edges) 返回 MST 总权值；图不连通（选不满 n-1 条边）时返回 None，对应 C++ 的 "orz"。
# 边按权升序处理，只按 w 排序；C++ 的 long long 累加在 Python 里是任意精度 int，不会溢出。

from typing import NamedTuple


class Edge(NamedTuple):
    """无向边 u-v，权值 w；对应 C++ 的 Edge 结构（operator< 只看 w，Python 用 key 排序）。"""

    u: int
    v: int
    w: int


class DSU:
    """parent[x] 为 x 的父节点；只做路径压缩，不按秩合并，与 C++ 版行为一致。"""

    parent: list[int]

    def __init__(self, n: int = 0) -> None:
        self.init(n)

    def init(self, n: int) -> None:
        # parent[i] = i，对应 C++ 的 iota(parent.begin(), parent.end(), 0)。
        self.parent = list(range(n + 1))

    def find(self, x: int) -> int:
        # 本版 merge 不按秩合并，链最坏可达 n，递归写法会爆 Python 栈，故改两趟迭代。
        root = x
        while self.parent[root] != root:
            root = self.parent[root]
        while self.parent[x] != root:
            self.parent[x], x = root, self.parent[x]
        return root

    def merge(self, a: int, b: int) -> bool:
        """合并 a、b 所在集合，返回是否真的合并（同集合返回 False）。"""
        fa = self.find(a)
        fb = self.find(b)
        if fa == fb:
            return False
        self.parent[fa] = fb
        return True


def kruskal(n: int, edges: list[Edge]) -> int | None:
    """返回最小生成树总权值；图不连通时返回 None（对应 C++ 输出 orz）。"""
    # Timsort 稳定、C++ std::sort 不稳定，但等权边先后不影响 MST 总权值，故不构成契约。
    order = sorted(edges, key=lambda e: e.w)
    dsu = DSU(n)
    answer = 0
    selected = 0

    for e in order:
        if not dsu.merge(e.u, e.v):
            continue
        answer += e.w
        selected += 1
        if selected == n - 1:
            break  # n=1 时 n-1=0，循环不进入，直接返回 0

    return answer if selected == n - 1 else None
