# 种类并查集（判二分图）：把每个点 x 拆成"本体 x"与"对立面 x+n"。
# 下标约定：点编号 1..n，并查集规模 2n；"x 与 y 异类" 转成合并 (x, y+n) 与 (x+n, y)。
# is_bipartite 对应 C++ main 的判定逻辑，单文件即可复制使用。

fa: list[int] = []  # fa[x] 为 x 的父节点
sz: list[int] = []  # sz[x] 为集合大小（仅根节点有效）


def init(n: int) -> None:
    """重置为 1..n 每个元素各自成集合。"""
    global fa, sz
    fa = list(range(n + 1))
    sz = [1] * (n + 1)


def find(x: int) -> int:
    """x 所在集合的根（路径压缩）。"""
    if fa[x] != x:
        fa[x] = find(fa[x])
    return fa[x]


def unite(x: int, y: int) -> bool:
    """小树挂大树，返回是否真的发生了合并。"""
    fx, fy = find(x), find(y)
    if fx == fy:
        return False
    if sz[fx] < sz[fy]:
        fx, fy = fy, fx
    fa[fy] = fx
    sz[fx] += sz[fy]
    return True


def is_bipartite(n: int, edges: list[tuple[int, int]]) -> bool:
    """n 个点、给定"必须异类"边集能否二染色；x 与 y 已同类即矛盾。

    点编号 1..n。调用示例：is_bipartite(3, [(1, 2), (2, 3), (1, 3)]) -> False
    """
    init(2 * n)  # 拆点：i 是点 i，i+n 是它的对立面
    for x, y in edges:
        if find(x) == find(y):
            return False  # 已有约束要求同类，新约束要求异类，矛盾
        unite(x, y + n)  # x 和 y 的对立面同类
        unite(x + n, y)  # x 的对立面和 y 同类
    return True
