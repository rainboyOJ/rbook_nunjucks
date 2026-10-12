# 链式前向星（struct 精简版）：h[u] 指向 u 的第一条出边，next 串起后续边，-1 表示链尾。
# h 用 defaultdict(-1) 模拟 memset(h,-1) 的定长数组；e 是动态边表，替代 e[maxe]。
# 接口：add / add2 加边，out_edges(u) 遍历点 u 的出边，-1 表示链尾。
# e[i] = (u, v, w, nxt) 对应 C++ 的 e[i].u / e[i].v / e[i].w / e[i].next。

from collections import defaultdict
from collections.abc import Iterator

# (u, v, w, nxt)：起点、终点、权值、同起点的下一条边编号
type Edge = tuple[int, int, int, int]


class LinkList:
    """构造后图为空；add 把新边插到 u 的链表头，-1 表示链尾。"""

    def __init__(self) -> None:
        self.e: list[Edge] = []
        self.h: defaultdict[int, int] = defaultdict(lambda: -1)

    def add(self, u: int, v: int, w: int = 0) -> None:
        self.e.append((u, v, w, self.h[u]))
        self.h[u] = len(self.e) - 1

    def add2(self, u: int, v: int, w: int = 0) -> None:
        self.add(u, v, w)
        self.add(v, u, w)

    def out_edges(self, u: int) -> Iterator[Edge]:
        """遍历点 u 的出边：for u_, v, w, nxt in e.out_edges(u)，头插法，后加的边先出。"""
        i = self.h[u]
        while i != -1:
            edge = self.e[i]
            yield edge
            i = edge[3]  # nxt


e = LinkList()
