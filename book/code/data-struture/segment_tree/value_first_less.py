# 区间最小值线段树（单点取 min 更新、区间查 min）。
# INF = 1e9 是查询无解时的哨兵，也是所有叶子的初值；update_min 只让值变小。
# 约定：区间 1 下标，调用 update_min/query_min 时须传 l=1, r=n（或等价区间）。
# 递归深度 O(log n)，n 达 1e5 也不会触碰默认递归上限。
# C++ 的 T 是 int、INF = 1e9，Python int 任意精度，不存在溢出。

type Tree = list[Node]  # 线段树数组，下标从 1 开始用


class Node:
    """线段树节点：value 为当前区间的最小值。"""

    value: int

    def __init__(self, value: int) -> None:
        self.value = value


class MinSegmentTree:
    """维护区间最小值的线段树，支持单点取 min 与区间查 min。"""

    INF: int = 10**9  # C++ static const T INF = 1e9

    n: int  # 区间大小
    tree: Tree  # 线段树数组

    def __init__(self, n: int = 0) -> None:
        self.init(n)

    def init(self, size: int) -> None:
        self.n = size
        self.tree = [Node(self.INF) for _ in range(size * 4 + 5)]

    @staticmethod
    def lson(p: int) -> int:
        return p << 1  # 左儿子 = 2p

    @staticmethod
    def rson(p: int) -> int:
        return p << 1 | 1  # 右儿子 = 2p+1（<< 优先级高于 |，等价于 (p<<1)|1）

    @staticmethod
    def mid(l: int, r: int) -> int:
        return (l + r) >> 1  # 中点 = floor((l+r)/2)

    def push_up(self, p: int) -> None:
        """用两个孩子的最小值合并出当前节点。"""
        self.tree[p].value = min(self.tree[self.lson(p)].value, self.tree[self.rson(p)].value)

    def update_min(self, pos: int, value: int, l: int, r: int, p: int = 1) -> None:
        """单点取 min：把位置 pos 的值更新为 min(原值, value)。"""
        if l == r:
            self.tree[p].value = min(self.tree[p].value, value)
            return
        m = self.mid(l, r)
        if pos <= m:
            self.update_min(pos, value, l, m, self.lson(p))
        else:
            self.update_min(pos, value, m + 1, r, self.rson(p))
        self.push_up(p)

    def query_min(self, ql: int, qr: int, l: int, r: int, p: int = 1) -> int:
        """区间查询：[ql, qr] 的最小值，空区间返回 INF。"""
        if ql > qr:
            return self.INF
        if ql <= l and r <= qr:
            return self.tree[p].value

        m = self.mid(l, r)
        answer = self.INF
        if ql <= m:
            answer = min(answer, self.query_min(ql, qr, l, m, self.lson(p)))
        if qr > m:
            answer = min(answer, self.query_min(ql, qr, m + 1, r, self.rson(p)))
        return answer
