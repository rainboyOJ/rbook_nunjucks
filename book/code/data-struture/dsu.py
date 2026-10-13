# 并查集：合并两个集合，查询两个元素是否在同一集合。
# 下标约定：元素编号 1..n，fa/sz 都开 n+1。
# 用法：init(n); unite(1, 3); same(1, 3) -> True
# find 路径压缩 + unite 按大小合并；递归深度最坏 O(log n)，安全。

fa: list[int] = []  # fa[x] 为 x 的父节点
sz: list[int] = []  # sz[x] 为集合大小（仅根节点有效）


def init(n: int) -> None:
    """重置为 1..n 每个元素各自成集合。"""
    global fa, sz
    fa = list(range(n + 1))  # fa[i] = i
    sz = [1] * (n + 1)


def find(x: int) -> int:
    """x 所在集合的根，沿途节点直接挂到根上（路径压缩）。"""
    if fa[x] != x:
        fa[x] = find(fa[x])
    return fa[x]


def same(x: int, y: int) -> bool:
    """x 与 y 是否在同一集合。"""
    return find(x) == find(y)


def unite(x: int, y: int) -> bool:
    """小树挂大树，返回是否真的发生了合并。

    易错点：先比大小再决定谁挂谁，写反会退化成链。
    """
    fx, fy = find(x), find(y)
    if fx == fy:
        return False
    if sz[fx] < sz[fy]:
        fx, fy = fy, fx
    fa[fy] = fx
    sz[fx] += sz[fy]
    return True
