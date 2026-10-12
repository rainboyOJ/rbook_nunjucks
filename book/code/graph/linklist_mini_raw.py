# 链式前向星（raw 数组版）：head[u] 指向 u 最近加入的一条出边，0 表示没有出边。
# e 是边表，下标就是边编号；e[i] = (to, nxt, w) 对应 C++ 的 e[i].to / e[i].next / e[i].w。
# 0 号是哨兵，add_edge 加有向边、add_edge2 加无向边，边从 1 号开始编号。
# 遍历 u 的出边：i = head[u]，while i 时取 e[i] 的 to / w，再 i = nxt，走完 0 结束。

from collections import defaultdict

# (to, nxt, w)：终点、同起点的下一条边编号、权值
type Edge = tuple[int, int, int]

# 等价于 memset(head, 0)：读没出现过的点得到 0
head: defaultdict[int, int] = defaultdict(int)

# 0 号哨兵，让 head[u] == 0 能安全表示「无出边」
e: list[Edge] = [(0, 0, 0)]


def add_edge(u: int, v: int, weight: int = 1) -> None:
    """加有向边 u -> v：新边头插到 u 的链上，u == v 时也不会出错。"""
    e.append((v, head[u], weight))
    head[u] = len(e) - 1


def add_edge2(u: int, v: int, weight: int = 1) -> None:
    """加无向边 u - v：两个方向各插一条（对应 C++ 的 add2）。"""
    add_edge(u, v, weight)
    add_edge(v, u, weight)
