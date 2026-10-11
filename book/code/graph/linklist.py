# 链式前向星：head[u] 指向 u 最近加入的一条出边，边的 nxt 串起更早的边，None 表示链尾。
# 只在需要「边有稳定编号」时用它（网络流的 i ^ 1 反向边、给边打标记）；日常存图用邻接表 graph_adj.py。
# edges 是动态边表，等价于 C++ 的 e[maxe] + edge_cnt，边下标就是边编号；权值 w 是 Python int，不会溢出。

from collections.abc import Iterator
from dataclasses import dataclass


@dataclass(slots=True)
class Edge:
    """一条边：起点 u、终点 v、权值 w、同起点的下一条边下标 nxt（None 表示链尾）。"""

    u: int
    v: int
    w: int = 0
    nxt: int | None = None


class LinkList:
    """reset() 后所有点都没有出边；add/add_undirected 插到对应点链表头部。"""

    def __init__(self) -> None:
        self.reset()

    def reset(self) -> None:
        """清空整张图（对应 C++ 的 edge_cnt=0 + memset(head,-1)，多测记得调用）。"""
        self.edges: list[Edge] = []
        # 普通 dict：读用 get，不会像 defaultdict 那样一读就插入键，孤立点不占内存
        self.head: dict[int, int] = {}

    def add(self, u: int, v: int, w: int = 0) -> None:
        """加有向边 u->v，O(1) 插到 u 的链表头，返回前边数加一。"""
        self.edges.append(Edge(u, v, w, self.head.get(u)))
        self.head[u] = len(self.edges) - 1

    def add_undirected(self, u: int, v: int, w: int = 0) -> None:
        """加无向边 u<->v，等价于两条方向相反的有向边（对应 C++ 的 add2）。"""
        self.add(u, v, w)
        self.add(v, u, w)

    def out_edges(self, u: int) -> Iterator[Edge]:
        """for edge in g.out_edges(u) 遍历 u 的出边，最近加入的边优先（头插法）。"""
        i = self.head.get(u)
        while i is not None:
            edge = self.edges[i]
            yield edge
            i = edge.nxt

    def adj(self, u: int) -> Iterator[int]:
        """for v in g.adj(u) 只遍历 u 的出边终点。"""
        for edge in self.out_edges(u):
            yield edge.v


e = LinkList()
