# 完整程序：读入 n、m 和 m 条无向边，输出 MST 总权值；图不连通时输出 orz。
import sys

type Edge = tuple[int, int, int]  # (起点 u, 终点 v, 边权 w)


def find(parent: list[int], x: int) -> int:
    """找 x 所在集合的根，顺路把路径上的点挂到根上（迭代压缩，不受递归深度限制）。"""
    root = x
    while parent[root] != root:
        root = parent[root]
    while parent[x] != root:
        parent[x], x = root, parent[x]
    return root


def kruskal(n: int, edges: list[Edge]) -> int | None:
    """返回 MST 总权值，图不连通时返回 None；sorted 生成新列表，不打乱调用者的边表。"""
    parent = list(range(n + 1))  # parent[x]：x 的父节点，x 是根时等于 x
    size = [1] * (n + 1)         # size[x]：x 是根时该集合的点数

    total = selected = 0
    for u, v, w in sorted(edges, key=lambda edge: edge[2]):  # 边权小的先试
        root_u, root_v = find(parent, u), find(parent, v)
        if root_u == root_v:  # 两端已经连通，选它就会成环
            continue
        if size[root_u] < size[root_v]:  # 小集合接到大集合下面
            root_u, root_v = root_v, root_u
        parent[root_v] = root_u
        size[root_u] += size[root_v]
        total += w
        selected += 1
        if selected == n - 1:  # 生成树正好 n - 1 条边，选满就停
            break
    return total if selected == n - 1 else None


def solve() -> None:
    data = iter(map(int, sys.stdin.buffer.read().split()))
    n, m = next(data), next(data)
    edges: list[Edge] = [(next(data), next(data), next(data)) for _ in range(m)]

    answer = kruskal(n, edges)
    # 0 也是合法答案，不能写成 if not answer。
    print("orz" if answer is None else answer)


if __name__ == "__main__":
    solve()
