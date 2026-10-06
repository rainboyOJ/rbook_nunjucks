# 链式前向星（struct 精简版）：h[u] 指向 u 的第一条出边，next 串起后续边，-1 表示链尾。
# h 用 defaultdict(-1) 模拟 memset(h,-1) 的定长数组；e 是动态边表，替代 e[maxe]。
# 本文件只保留 add / add2 两个接口，遍历请直接走 h[u] 与 e[i].next。

from collections import defaultdict


class Edge:
    """一条边：u 起点、v 终点、w 权值、next 同起点的下一条边编号。"""

    __slots__ = ("u", "v", "w", "next")

    def __init__(self, u: int, v: int, w: int, next: int) -> None:
        self.u = u
        self.v = v
        self.w = w
        self.next = next


class LinkList:
    """构造后图为空；add 把新边插到 u 的链表头。"""

    edge_cnt: int
    e: list[Edge]
    h: defaultdict[int, int]

    def __init__(self) -> None:
        self.edge_cnt = 0
        self.e = []
        self.h = defaultdict(lambda: -1)

    def add(self, u: int, v: int, w: int = 0) -> None:
        self.e.append(Edge(u, v, w, self.h[u]))
        self.h[u] = self.edge_cnt
        self.edge_cnt += 1

    def add2(self, u: int, v: int, w: int = 0) -> None:
        self.add(u, v, w)
        self.add(v, u, w)


e = LinkList()
