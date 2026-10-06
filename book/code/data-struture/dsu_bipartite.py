# 种类并查集（判二分图）：把每个点 x 拆成"本体 x"与"对立面 x+n"两个身份。
# 下标约定：点编号 1..n，并查集规模 2n；约束 "x 与 y 异类" 转成合并 (x, y+n) 与 (x+n, y)。
# is_bipartite 对应 C++ main 的判定逻辑；DSU 与 dsu.cpp 完全一致，此处一并给出以便单文件复制。
# C++ 用 int 计数，Python int 任意精度，不存在溢出问题。

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


def is_bipartite(n: int, edges: list[tuple[int, int]]) -> bool:
    """判断 n 个点、给定"必须异类"边集能否二染色。

    edges 中每个 (x, y) 表示 x 与 y 必须属于不同类别；一旦发现 x 与 y 已同类即冲突。
    点编号 1..n。调用示例：is_bipartite(3, [(1, 2), (2, 3), (1, 3)]) -> False
    """
    dsu = DSU(2 * n)

    def enemy(x: int) -> int:
        # 对立面身份：点 x 的对立面编号为 x + n。
        return x + n

    ok = True
    for x, y in edges:
        if dsu.same(x, y):
            # 已有约束要求 x、y 同类，新约束要求异类，矛盾。
            ok = False
        dsu.unite(x, enemy(y))
        dsu.unite(enemy(x), y)
    return ok
