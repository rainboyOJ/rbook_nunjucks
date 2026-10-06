# 并查集：合并两个集合，查询两个元素是否在同一集合。
# 下标约定：元素编号 1..n（fa/sz 都开 n+1，下标 0 是自环哨兵，无实际含义）。
# 调用示例：d = DSU(5); d.unite(1, 3); d.same(1, 3)  ->  True
# find 采用路径压缩 + unite 按大小合并，树高 O(log n)，递归深度安全。

type Seq = list[int]  # fa / sz 这类长度 n+1 的 int 序列


class DSU:
    """按大小合并 + 路径压缩的并查集。"""

    fa: Seq  # fa[x] 为 x 的父节点
    sz: Seq  # sz[x] 为集合大小（仅根节点有效）

    def __init__(self, n: int = 0) -> None:
        self.init(n)

    def init(self, n: int) -> None:
        """重置为 1..n 每个元素各自成集合，方便同一个对象换一组数据复用。"""
        self.fa = list(range(n + 1))  # 对应 C++ 的 iota，fa[i] = i
        self.sz = [1] * (n + 1)

    def find(self, x: int) -> int:
        """查询 x 所在集合的根（路径压缩：沿途节点直接挂到根上）。"""
        if self.fa[x] == x:
            return x
        self.fa[x] = self.find(self.fa[x])
        return self.fa[x]

    def same(self, x: int, y: int) -> bool:
        """x 与 y 是否在同一集合。"""
        return self.find(x) == self.find(y)

    def unite(self, x: int, y: int) -> bool:
        """合并 x 与 y 所在集合（按大小合并），返回是否发生合并。

        易错点：必须先比较两棵树的大小再决定谁挂谁，写反会退化链。
        """
        fx = self.find(x)
        fy = self.find(y)
        if fx == fy:
            return False

        if self.sz[fx] < self.sz[fy]:
            fx, fy = fy, fx
        self.fa[fy] = fx
        self.sz[fx] += self.sz[fy]
        return True
