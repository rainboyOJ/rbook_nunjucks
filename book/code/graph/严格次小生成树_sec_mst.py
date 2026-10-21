# 严格次小生成树：点编号 1..n，边表是 (u, v, w) 三元组。
# strict_second_mst 返回严格次小生成树权值；无生成树或无严格次小生成树时返回 -1。
# 做法：先 Kruskal 求 MST，再倍增维护树上路径的「最大、严格次大」边权，
# 最后枚举每条非树边做一次换边。边权须大于 NEG；Python 的 int 不会溢出。

import sys
from collections.abc import Iterable

NEG = -(1 << 60)  # 「不存在」哨兵，须小于任何合法边权
INF = 1 << 62     # 答案上界哨兵，须大于任何合法答案

type Edge = tuple[int, int, int]        # (起点 u, 终点 v, 边权 w)
type Pair = tuple[int, int]             # (最大边权, 严格次大边权)，缺失分量是 NEG
type Tbl = list[list[int]]              # 一张倍增表：第 k 行是跳 2^k 步的信息
type Adj = list[list[tuple[int, int]]]  # tree[u] = [(邻点 v, 边权 w), ...]
type Prepared = tuple[list[int], Tbl, Tbl, Tbl]  # (depth, up, mx1, mx2) 四张倍增表


def find(parent: list[int], x: int) -> int:
    """找 x 所在集合的根，顺路把路径上的点挂到根上（迭代压缩，不受递归深度限制）。"""
    root = x
    while parent[root] != root:
        root = parent[root]
    while parent[x] != root:
        parent[x], x = root, parent[x]
    return root


def two_largest(values: Iterable[int]) -> Pair:
    """候选边权里的最大与严格次大：重复值只留一份，缺失分量取 NEG。"""
    largest = second = NEG
    for x in values:
        if x > largest:
            largest, second = x, largest
        elif second < x < largest:
            second = x
    return largest, second


def mst_edges(n: int, edges: list[Edge]) -> set[int] | None:
    """Kruskal：返回被选入 MST 的边下标集合；图不连通时返回 None。"""
    parent = list(range(n + 1))
    size = [1] * (n + 1)
    chosen: set[int] = set()

    for i in sorted(range(len(edges)), key=lambda i: edges[i][2]):  # 边权小的先试
        u, v, _ = edges[i]
        root_u, root_v = find(parent, u), find(parent, v)
        if root_u == root_v:  # 两端已经连通，选它就会成环
            continue
        if size[root_u] < size[root_v]:  # 小集合接到大集合下面
            root_u, root_v = root_v, root_u
        parent[root_v] = root_u
        size[root_u] += size[root_v]
        chosen.add(i)
        if len(chosen) == n - 1:  # 生成树正好 n - 1 条边，选满就停
            break
    return chosen if len(chosen) == n - 1 else None


def prepare_lca(n: int, tree: Adj) -> Prepared:
    """倍增预处理：depth[u] 是深度，up[k][u] 跳 2^k 步，mx1/mx2 是这一段的最大/次大边权。"""
    lg = max(1, n.bit_length())  # 最小的使 2^lg > n 的幂次，n = 1 时 lg = 1
    depth = [0] * (n + 1)
    up = [[0] * (n + 1) for _ in range(lg)]
    # 两表分开存顶点权重的整数，比每格存一个 (最大, 次大) 元组省下大量元组头开销。
    mx1 = [[NEG] * (n + 1) for _ in range(lg)]
    mx2 = [[NEG] * (n + 1) for _ in range(lg)]

    # 建树遍历用显式栈：树链最坏 n 深，递归会爆 Python 栈；树无环，靠父节点挡回头路。
    depth[1] = 1  # 根深度从 1 起，点 0 是「祖先不存在」的黑洞
    stack = [1]
    while stack:
        u = stack.pop()
        for v, w in tree[u]:
            if v == up[0][u]:
                continue
            depth[v] = depth[u] + 1
            up[0][v] = u
            mx1[0][v] = w  # 只往上走一步，没有次大边（mx2 保持 NEG）
            stack.append(v)

    for k in range(1, lg):
        for u in range(1, n + 1):
            mid = up[k - 1][u]  # 先跳 2^(k-1) 步到达中点，把前后两段的最值合起来
            up[k][u] = up[k - 1][mid]
            mx1[k][u], mx2[k][u] = two_largest(
                (mx1[k - 1][u], mx2[k - 1][u], mx1[k - 1][mid], mx2[k - 1][mid])
            )

    return depth, up, mx1, mx2


def path_max_two(a: int, b: int, prepared: Prepared) -> Pair:
    """树上 a-b 路径的 (最大边权, 严格次大边权)；缺失分量用 NEG。"""
    depth, up, mx1, mx2 = prepared
    answer: Pair = (NEG, NEG)

    if depth[a] < depth[b]:
        a, b = b, a
    for k in range(len(up)):
        if (depth[a] - depth[b]) >> k & 1:  # 差的二进制第 k 位为 1 就跳 2^k 步
            answer = two_largest((*answer, mx1[k][a], mx2[k][a]))
            a = up[k][a]

    if a == b:  # 其中一个点是另一个的祖先，路径已经走完
        return answer

    for k in range(len(up) - 1, -1, -1):
        if up[k][a] != up[k][b]:  # 还没跳到 LCA 就两个点一起往上跳
            answer = two_largest((*answer, mx1[k][a], mx2[k][a], mx1[k][b], mx2[k][b]))
            a, b = up[k][a], up[k][b]

    # 此时 a、b 是 LCA 的两个孩子，再补上各自到 LCA 的那条边。
    return two_largest((*answer, mx1[0][a], mx2[0][a], mx1[0][b], mx2[0][b]))


def strict_second_mst(n: int, edges: list[Edge]) -> int:
    """返回严格次小生成树权值；无生成树或无严格次小生成树时返回 -1。"""
    chosen = mst_edges(n, edges)
    if chosen is None:  # 图不连通，没有生成树
        return -1

    tree: Adj = [[] for _ in range(n + 1)]
    mst = 0
    for i in chosen:
        u, v, w = edges[i]
        mst += w
        tree[u].append((v, w))
        tree[v].append((u, w))

    prepared = prepare_lca(n, tree)

    answer = INF
    for i, (u, v, w) in enumerate(edges):
        if i in chosen:  # 树边不能替换自己
            continue
        largest, second_largest = path_max_two(u, v, prepared)
        # 要严格变小只能删严格小于 w 的边：优先删最大，最大等于 w 时退而求次大。
        removed = NEG
        if largest < w:
            removed = largest
        elif second_largest < w:
            removed = second_largest
        if removed != NEG:
            answer = min(answer, mst + w - removed)

    return -1 if answer == INF else answer


def solve() -> None:
    data = iter(map(int, sys.stdin.buffer.read().split()))
    n, m = next(data), next(data)
    edges: list[Edge] = [(next(data), next(data), next(data)) for _ in range(m)]
    print(strict_second_mst(n, edges))


if __name__ == "__main__":
    solve()
