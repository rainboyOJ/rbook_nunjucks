# 无向图欧拉路 / 欧拉回路（Hierholzer 迭代版）：返回顶点序列，无解返回 None，无边返回 []。
# 边以 (u, v) 列表传入，编号按下标 1..m；点编号 1..n；自环度数加 2，两条平行边构成回路。
# 判定：先连通（忽略孤立点），再要求奇度点个数为 0（回路）或 2（路径）。
# 全孤立点（m == 0）返回 []，对应 C++ 输出空行；全程迭代，无递归深度问题。

type RawEdge = tuple[int, int]  # 无向边 (u, v)


def euler_path_undirected(n: int, edges: list[RawEdge]) -> list[int] | None:
    """返回一条恰好用完所有边的顶点序列；不存在欧拉路时返回 None。"""
    m = len(edges)
    g: list[list[tuple[int, int]]] = [[] for _ in range(n + 1)]  # g[u] 存 (邻点, 边号)
    deg = [0] * (n + 1)

    for eid in range(1, m + 1):
        u, v = edges[eid - 1]
        g[u].append((v, eid))
        g[v].append((u, eid))
        if u == v:
            deg[u] += 2  # 自环给 u 贡献两条「半条边」，度数为偶数
        else:
            deg[u] += 1
            deg[v] += 1

    first = -1
    for i in range(1, n + 1):
        if deg[i] > 0:
            first = i
            break
    if first == -1:
        return []  # 一条边都没有：空序列

    vis = [False] * (n + 1)
    st = [first]
    vis[first] = True
    while st:
        u = st.pop()
        for v, _ in g[u]:
            if not vis[v]:
                vis[v] = True
                st.append(v)

    for i in range(1, n + 1):
        if deg[i] > 0 and not vis[i]:
            return None  # 有边的点不在同一连通块

    odd_count = 0
    start = first
    for i in range(1, n + 1):
        if deg[i] % 2 == 1:
            odd_count += 1
            start = i  # 有奇度点时以最后一个奇度点为起点（C++ 同此写法）

    if odd_count != 0 and odd_count != 2:
        return None  # 奇度点只能有 0 或 2 个

    used = [False] * (m + 1)
    it = [0] * (n + 1)
    stack_path = [start]
    path: list[int] = []
    while stack_path:
        u = stack_path[-1]
        while it[u] < len(g[u]) and used[g[u][it[u]][1]]:
            it[u] += 1
        if it[u] == len(g[u]):
            path.append(u)
            stack_path.pop()
        else:
            v, eid = g[u][it[u]]
            it[u] += 1
            used[eid] = True
            stack_path.append(v)

    if len(path) != m + 1:
        return None  # 兜底：没走完所有边
    path.reverse()
    return path
