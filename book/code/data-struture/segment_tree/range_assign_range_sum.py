# 区间赋值 + 区间求和线段树（懒标记，1 下标区间 [1, n]）。
# 懒标记不变量：若 tree[p].has_lazy 为真，则 p 的整个区间被统一赋为 tree[p].lazy，
# 但两个孩子的 value 尚未更新；push_down 后才把标记下传并清空。
# 递归深度为线段树高度 O(log n)，不会触及默认递归上限。
# C++ 用 long long 存区间和与赋值防溢出；Python int 任意精度，该坑不存在。


class Node:
    """线段树节点：value 为真实区间和，lazy 为待下传赋值，has_lazy 标记是否有未下传赋值。"""

    __slots__ = ("value", "lazy", "has_lazy")

    def __init__(self, value: int = 0, lazy: int = 0, has_lazy: bool = False) -> None:
        self.value = value
        self.lazy = lazy
        self.has_lazy = has_lazy

    def __add__(self, other: "Node") -> "Node":
        """合并两个孩子：区间和相加，合并结果不携带懒标记（对应 C++ 的 operator+）。"""
        return Node(self.value + other.value, 0, False)


class SegmentTreeRangeAssign:
    """tree[p] 为节点 p 的信息；build/assign_range/query 的 l、r、p 与 C++ 同参。"""

    def __init__(self, n: int = 0) -> None:
        self.n = 0
        self.tree: list[Node] = []
        self.init(n)

    def init(self, size: int) -> None:
        """重置为区间 [1, size]，方便同一个对象换一组数据复用。"""
        self.n = size
        # 必须每个位置新建独立 Node，不能 [Node()] * k 共享同一对象。
        self.tree = [Node() for _ in range(size * 4 + 5)]

    @staticmethod
    def lson(p: int) -> int:
        return p << 1  # 左孩子编号 2p

    @staticmethod
    def rson(p: int) -> int:
        return p << 1 | 1  # 右孩子编号 2p+1

    @staticmethod
    def mid(l: int, r: int) -> int:
        return (l + r) >> 1  # 区间中点，等价 (l+r)//2 且不会溢出

    def push_up(self, p: int) -> None:
        """上推：父节点 = 两个孩子合并。"""
        self.tree[p] = self.tree[self.lson(p)] + self.tree[self.rson(p)]

    def apply(self, p: int, l: int, r: int, value: int) -> None:
        """把节点 p 的整个区间 [l, r] 赋值为 value（区间和 = value * 长度）。"""
        self.tree[p].value = value * (r - l + 1)
        self.tree[p].lazy = value
        self.tree[p].has_lazy = True

    def push_down(self, p: int, l: int, r: int) -> None:
        """下推懒标记到两个孩子；叶子没有孩子，直接返回。"""
        if not self.tree[p].has_lazy or l == r:
            return

        m = self.mid(l, r)
        self.apply(self.lson(p), l, m, self.tree[p].lazy)
        self.apply(self.rson(p), m + 1, r, self.tree[p].lazy)
        self.tree[p].has_lazy = False

    def build(self, a: list[int], l: int, r: int, p: int = 1) -> None:
        """用 1 下标数组 a 的 [l, r] 建树。"""
        if l == r:
            self.tree[p].value = a[l]
            return
        m = self.mid(l, r)
        self.build(a, l, m, self.lson(p))
        self.build(a, m + 1, r, self.rson(p))
        self.push_up(p)

    def assign_range(self, ql: int, qr: int, value: int, l: int, r: int, p: int = 1) -> None:
        """把 [ql, qr] 全部赋值为 value。"""
        if ql <= l and r <= qr:
            self.apply(p, l, r, value)
            return

        self.push_down(p, l, r)
        m = self.mid(l, r)
        if ql <= m:
            self.assign_range(ql, qr, value, l, m, self.lson(p))
        if qr > m:
            self.assign_range(ql, qr, value, m + 1, r, self.rson(p))
        self.push_up(p)

    def query(self, ql: int, qr: int, l: int, r: int, p: int = 1) -> int:
        """返回 [ql, qr] 的区间和。"""
        if ql <= l and r <= qr:
            return self.tree[p].value

        self.push_down(p, l, r)
        m = self.mid(l, r)
        answer = 0
        if ql <= m:
            answer += self.query(ql, qr, l, m, self.lson(p))
        if qr > m:
            answer += self.query(ql, qr, m + 1, r, self.rson(p))
        return answer
