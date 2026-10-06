# 扫描线求矩形面积并（不离散化版）：线段树叶子代表横坐标上的一个单位位置。
# 线段树只维护覆盖次数 cover 与覆盖长度 covered_len，cover 不做下传，push_up 时
# 若 cover > 0 直接取区间长度 r-l+1。
# 约定：x 坐标范围在 [1, max_x]，线段树区间为 1 下标；空矩形（x1 > x2）由调用方
# 判 e.x1 <= e.x2 后再 add。C++ 的 ans 用 long long 防溢出，Python int 任意精度。


class Event:
    """扫描线事件：一条竖直边，delta = 1 为入边，-1 为出边。"""

    x1: int
    x2: int
    y: int
    delta: int

    def __init__(self, x1: int, x2: int, y: int, delta: int) -> None:
        self.x1 = x1
        self.x2 = x2
        self.y = y
        self.delta = delta

    def __lt__(self, other: "Event") -> bool:
        # C++ operator< 只按 y 比较；同 y 事件的先后不影响面积累加（组内 dy 恒为 0）。
        return self.y < other.y


class Node:
    """线段树节点：cover 为区间被完整覆盖的次数，covered_len 为区间被覆盖的长度。"""

    cover: int
    covered_len: int

    def __init__(self) -> None:
        self.cover = 0
        self.covered_len = 0


class ScanlineSegmentTree:
    """不离散化扫描线线段树，坐标范围由构造参数 n 给出。"""

    n: int  # 坐标范围
    tree: list[Node]  # 线段树数组

    def __init__(self, n: int = 0) -> None:
        self.init(n)

    def init(self, size: int) -> None:
        self.n = size
        self.tree = [Node() for _ in range(size * 4 + 5)]

    @staticmethod
    def lson(p: int) -> int:
        return p << 1  # 左儿子 = 2p

    @staticmethod
    def rson(p: int) -> int:
        return p << 1 | 1  # 右儿子 = 2p+1（<< 优先级高于 |，等价于 (p<<1)|1）

    @staticmethod
    def mid(l: int, r: int) -> int:
        return (l + r) >> 1  # 中点 = floor((l+r)/2)

    def push_up(self, p: int, l: int, r: int) -> None:
        if self.tree[p].cover > 0:
            # 整段被覆盖，直接用区间长度，无需看孩子。
            self.tree[p].covered_len = r - l + 1
        elif l == r:
            self.tree[p].covered_len = 0
        else:
            self.tree[p].covered_len = (
                self.tree[self.lson(p)].covered_len + self.tree[self.rson(p)].covered_len
            )

    def add(self, ql: int, qr: int, v: int, l: int, r: int, p: int = 1) -> None:
        """区间加覆盖次数：给 [ql, qr] 增加 v（v = 1 或 -1）。"""
        if ql <= l and r <= qr:
            self.tree[p].cover += v
            self.push_up(p, l, r)
            return
        m = self.mid(l, r)
        if ql <= m:
            self.add(ql, qr, v, l, m, self.lson(p))
        if m < qr:
            self.add(ql, qr, v, m + 1, r, self.rson(p))
        self.push_up(p, l, r)

    def query_all(self) -> int:
        """整棵树的覆盖长度。"""
        return self.tree[1].covered_len
