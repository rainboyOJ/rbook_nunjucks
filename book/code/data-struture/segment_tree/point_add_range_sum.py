# 单点加 + 区间求和线段树（1 下标区间 [1, n]）。
# C++ 的 Node 只含一个 value，Python 直接用 int 存 tree，外部接口与语义不变。
# 递归深度为线段树高度 O(log n)，不会触及默认递归上限。
# C++ 用 long long 存区间和防溢出；Python int 任意精度，该坑不存在。


class SegmentTreePointAdd:
    """tree[p] 为节点 p 管辖区间的和；build/add/query 的 l、r、p 与 C++ 同参。"""

    def __init__(self, n: int = 0) -> None:
        self.n = 0
        self.tree: list[int] = []
        self.init(n)

    def init(self, size: int) -> None:
        """重置为区间 [1, size]，方便同一个对象换一组数据复用。"""
        self.n = size
        self.tree = [0] * (size * 4 + 5)

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
        """上推：父节点区间和 = 两个孩子之和。"""
        self.tree[p] = self.tree[self.lson(p)] + self.tree[self.rson(p)]

    def build(self, a: list[int], l: int, r: int, p: int = 1) -> None:
        """用 1 下标数组 a 的 [l, r] 建树。"""
        if l == r:
            self.tree[p] = a[l]
            return
        m = self.mid(l, r)
        self.build(a, l, m, self.lson(p))
        self.build(a, m + 1, r, self.rson(p))
        self.push_up(p)

    def add(self, pos: int, value: int, l: int, r: int, p: int = 1) -> None:
        """单点加：a[pos] += value。"""
        if l == r:
            self.tree[p] += value
            return
        m = self.mid(l, r)
        if pos <= m:
            self.add(pos, value, l, m, self.lson(p))
        else:
            self.add(pos, value, m + 1, r, self.rson(p))
        self.push_up(p)

    def query(self, ql: int, qr: int, l: int, r: int, p: int = 1) -> int:
        """返回 [ql, qr] 的区间和。"""
        if ql <= l and r <= qr:
            return self.tree[p]

        m = self.mid(l, r)
        answer = 0
        if ql <= m:
            answer += self.query(ql, qr, l, m, self.lson(p))
        if qr > m:
            answer += self.query(ql, qr, m + 1, r, self.rson(p))
        return answer
