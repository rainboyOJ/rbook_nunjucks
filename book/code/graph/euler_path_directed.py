# 有向图欧拉路 / 欧拉回路（Hierholzer 迭代版）：返回顶点序列，无解返回 None，无边返回 []。
# 边以 (u, v) 列表传入，编号按下标 1..m；点编号 1..n。
# 判定：先弱连通（忽略孤立点），再要求所有点 in == out（回路），或恰有一个 out == in + 1 的起点
# 与一个 in == out + 1 的终点（路径）。全孤立点（m == 0）返回 []，对应 C++ 输出空行。
# 全程迭代，无递归深度问题。

type RawEdge = tuple[int, int]  # 有向边 (u, v)


def euler_path_directed(n: int, edges: list[RawEdge]) -> list[int] | None:
    """返回一条恰好用完所有边的顶点序列；不存在欧拉路时返回 None。"""
    m = len(edges)
    g: list[list[tuple[int, int]]] = [[] for _ in range(n + 1)]  # g[u] 存 (终点, 边号)
    weak: list[list[int]] = [[] for _ in range(n + 1)]  # 只看连通性用的无向邻接表
    indeg = [0] * (n + 1)
    outdeg = [0] * (n + 1)

    for eid in range(1, m + 1):
        u, v = edges[eid - 1]
        g[u].append((v, eid))
        weak[u].append(v)
        weak[v].append(u)
        outdeg[u] += 1
        indeg[v] += 1

    # 第一个有边的点，用来做连通性检查，也是回路时的默认起点。
    first = -1
    for i in range(1, n + 1):
        if indeg[i] + outdeg[i] > 0:
            first = i
            break
    if first == -1:
        return []  # 一条边都没有：空序列

    vis = [False] * (n + 1)
    st = [first]
    vis[first] = True
    while st:
        u = st.pop()
        for v in weak[u]:
            if not vis[v]:
                vis[v] = True
                st.append(v)

    for i in range(1, n + 1):
        if indeg[i] + outdeg[i] > 0 and not vis[i]:
            return None  # 有边的点不在同一弱连通块

    start_count = 0
    end_count = 0
    start = first
    for i in range(1, n + 1):
        if outdeg[i] == indeg[i] + 1:
            start_count += 1
            start = i
        elif indeg[i] == outdeg[i] + 1:
            end_count += 1
        elif indeg[i] != outdeg[i]:
            return None  # 度数差超过 1，不可能有欧拉路

    has_path = start_count == 1 and end_count == 1
    has_circuit = start_count == 0 and end_count == 0
    if not has_path and not has_circuit:
        return None

    used = [False] * (m + 1)
    it = [0] * (n + 1)  # 每个点下一条待尝试的出边下标
    stack_path = [start]
    path: list[int] = []
    while stack_path:
        u = stack_path[-1]
        while it[u] < len(g[u]) and used[g[u][it[u]][1]]:
            it[u] += 1
        if it[u] == len(g[u]):
            path.append(u)  # 没有未用出边了，回退时把它接到答案末尾
            stack_path.pop()
        else:
            v, eid = g[u][it[u]]
            it[u] += 1
            used[eid] = True
            stack_path.append(v)

    if len(path) != m + 1:
        return None  # 没走完所有边（理论上前面判定保证不会发生，这里兜底）
    path.reverse()  # 出栈顺序是逆序，反转后才是欧拉路
    return path
