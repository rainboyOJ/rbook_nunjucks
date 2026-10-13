# 并查集：合并两个集合，查询两个元素是否在同一集合。
# 下标约定：元素编号 1..n（fa/sz 都开 n+1，下标 0 不用）。
# 调用示例：d = DSU(5); d.unite(1, 3); d.same(1, 3) -> True
# find 路径压缩 + unite 按大小合并，树高 O(log n)，递归深度安全。


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
