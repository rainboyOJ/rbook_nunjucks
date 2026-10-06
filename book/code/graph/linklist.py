# 链式前向星：h[u] 指向 u 最近加入的一条出边，边的 next 串起更早的边，-1 表示链尾。
# h 用 defaultdict(-1) 模拟 memset(h,-1) 的定长数组：任意下标读出 -1，只对加过边的点占内存。
# e 是动态边表，等价于 C++ 的 e[maxe] + edge_cnt；权值 w 是 Python int，不会溢出。

from collections import defaultdict
from collections.abc import Callable, Iterator

type Neighbors = Iterator[int]  # 遍历某点所有出边终点得到的迭代器


class Edge:
    """一条边：起点 u、终点 v、权值 w、同起点的下一条边编号 next。"""

    __slots__ = ("u", "v", "w", "next")

    def __init__(self, u: int, v: int, w: int, next: int) -> None:
        self.u = u
        self.v = v
        self.w = w
        self.next = next


class LinkList:
    """reset() 后所有点都没有出边；add/add2 插到对应点链表头部。"""

    edge_cnt: int
    e: list[Edge]
    h: defaultdict[int, int]

    def __init__(self) -> None:
        self.reset()

    def reset(self) -> None:
        """清空整张图（对应 C++ 的 edge_cnt=0 + memset(h,-1)）。"""
        self.edge_cnt = 0
        self.e = []
        self.h = defaultdict(lambda: -1)

    def for_each(self, u: int, func: Callable[[int, int, int], None]) -> None:
        """对 u 的每条出边调用 func(u, v, w)，顺序是最近加入的边优先。"""
        i = self.h[u]
        while i != -1:
            edge = self.e[i]
            func(edge.u, edge.v, edge.w)
            i = edge.next

    def add(self, u: int, v: int, w: int = 0) -> None:
        """加有向边 u->v，O(1) 插到 u 的链表头。"""
        self.e.append(Edge(u, v, w, self.h[u]))
        self.h[u] = self.edge_cnt
        self.edge_cnt += 1

    def add2(self, u: int, v: int, w: int = 0) -> None:
        """加无向边 u<->v，等价于两条方向相反的有向边。"""
        self.add(u, v, w)
        self.add(v, u, w)

    def __getitem__(self, i: int) -> Edge:
        # 对应 C++ 的 operator[]：按下标直接访问边，返回可改字段的对象。
        return self.e[i]

    def __call__(self, u: int) -> Iterator[Edge]:
        # 对应 C++ 的 operator()(u)：for edge in list(u) 遍历 u 的出边。
        i = self.h[u]
        while i != -1:
            edge = self.e[i]
            yield edge
            i = edge.next

    def adj(self, u: int) -> Neighbors:
        """for v in list.adj(u) 只遍历 u 的出边终点。"""
        i = self.h[u]
        while i != -1:
            v = self.e[i].v
            yield v
            i = self.e[i].next


e = LinkList()
