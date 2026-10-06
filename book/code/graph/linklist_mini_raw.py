# 链式前向星（数组版）：head[u] == 0 表示无出边，0 号边是哨兵、add_edge 从 1 号开始编号。
# C++ 的 head 是全局零初始化定长数组；这里用 defaultdict(int) 模拟“任意下标默认 0”。
# add_edge 一次加两条反向边，权值默认 1；Python int 任意精度，权值不会溢出。

from collections import defaultdict

maxn = 10**6 + 5  # C++ 的规模上限，Python 只是保留这个名字，不预分配


class Edge:
    """数组版边：to 终点、next 同起点的下一条边、w 权值。"""

    __slots__ = ("to", "next", "w")

    def __init__(self, to: int, next: int, w: int) -> None:
        self.to = to
        self.next = next
        self.w = w


e: list[Edge] = [Edge(0, 0, 0)]  # 0 号哨兵，保证 head[u]==0 能安全当“无出边”
head: defaultdict[int, int] = defaultdict(int)
cnt = 0


def add_edge(u: int, v: int, w: int = 1) -> None:
    """加无向边 u-v；两条边编号相邻，但本模板不依赖 i^1 找反向边。"""
    global cnt
    cnt += 1
    e.append(Edge(v, head[u], w))
    head[u] = cnt
    cnt += 1
    e.append(Edge(u, head[v], w))  # 若 u==v，这里读到的 head[v] 是刚更新的 head[u]
    head[v] = cnt
