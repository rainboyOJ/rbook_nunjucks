# 种类并查集（判二分图）：把每个点 x 拆成"本体 x"与"对立面 x+n"。
# 下标约定：点编号 1..n，并查集规模 2n；"x 与 y 异类" 转成合并 (x, y+n) 与 (x+n, y)。
# is_bipartite 对应 C++ main 的判定逻辑；DSU 与 dsu.py 一致，单文件即可复制使用。


class DSU:
    """并查集 (Disjoint Set Union)"""

    def __init__(self, n: int):
        # 1-indexed, fa 初始指向自己，sz 初始为 1
        self.fa = list(range(n + 1))
        self.sz = [1] * (n + 1)

    def find(self, x: int) -> int:
        """查找根节点，并进行路径压缩"""
        if self.fa[x] != x:
            self.fa[x] = self.find(self.fa[x])
        return self.fa[x]

    def same(self, x: int, y: int) -> bool:
        """判断 x 和 y 是否属于同一个集合"""
        return self.find(x) == self.find(y)

    def unite(self, x: int, y: int) -> bool:
        """按大小合并两个集合 (小树挂大树)"""
        fx, fy = self.find(x), self.find(y)
        if fx == fy:
            return False

        # 确保 fx 始终是较大的树
        if self.sz[fx] < self.sz[fy]:
            fx, fy = fy, fx

        self.fa[fy] = fx
        self.sz[fx] += self.sz[fy]
        return True


def is_bipartite(n: int, edges: list[tuple[int, int]]) -> bool:
    """n 个点、给定"必须异类"边集能否二染色；x 与 y 已同类即矛盾。

    点编号 1..n。调用示例：is_bipartite(3, [(1, 2), (2, 3), (1, 3)]) -> False
    """
    dsu = DSU(2 * n)  # 拆点：i 是点 i，i+n 是它的对立面
    for x, y in edges:
        if dsu.same(x, y):
            return False  # 已有约束要求同类，新约束要求异类，矛盾
        dsu.unite(x, y + n)  # x 和 y 的对立面同类
        dsu.unite(x + n, y)  # x 的对立面和 y 同类
    return True
