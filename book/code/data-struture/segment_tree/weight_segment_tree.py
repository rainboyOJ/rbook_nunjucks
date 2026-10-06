# 权值线段树（动态开点）：按值域 [min_value, max_value] 统计每个值的出现次数。
# 支持插入/删除（add）、区间计数（query/count_less/count_leq）、第 k 小（kth）、
# 前驱 predecessor、后继 successor。值域很大但只在实际用到的位置开点。
# 约定：kth 的 k 从 1 开始；前驱/后继在不存在时按 C++ 语义分别返回 min_value / max_value。
# 节点 0 是空哨兵（left = right = sum = 0），故 root == 0 表示空树。
# 递归深度 O(log(max_value - min_value))，2e7 值域约 25 层，安全。
# C++ 的 sum 用 int 计数，Python int 任意精度。

type Tree = list[Node]  # 动态节点池，节点下标即指针，下标 0 为空哨兵


class Node:
    """线段树节点：left/right 为孩子下标（0 表示空），sum 为值域区间内的元素个数。"""

    left: int
    right: int
    sum: int

    def __init__(self) -> None:
        self.left = 0
        self.right = 0
        self.sum = 0


class WeightSegmentTree:
    """按值域计数的动态开点权值线段树。"""

    tree: Tree  # 动态节点池，节点下标即指针
    root: int  # 根节点下标
    min_value: int  # 值域下界
    max_value: int  # 值域上界

    def __init__(self, min_value: int, max_value: int) -> None:
        self.min_value = min_value
        self.max_value = max_value
        self.root = 0
        self.tree = [Node()]  # 0 号节点为空节点

    @staticmethod
    def mid(l: int, r: int) -> int:
        return (l + r) >> 1  # 中点 = floor((l+r)/2)

    def new_node(self) -> int:
        """新建一个空节点，返回其下标。"""
        self.tree.append(Node())
        return len(self.tree) - 1

    def _add(self, u: int, l: int, r: int, pos: int, delta: int) -> int:
        """在值域区间 [l, r] 的节点 u 上把位置 pos 的个数增加 delta，返回子树根。

        C++ 用 int& 引用出参回写孩子指针，Python 改为返回新根由调用方接住。
        """
        if u == 0:
            u = self.new_node()
        self.tree[u].sum += delta
        if l == r:
            return u

        m = self.mid(l, r)
        if pos <= m:
            self.tree[u].left = self._add(self.tree[u].left, l, m, pos, delta)
        else:
            self.tree[u].right = self._add(self.tree[u].right, m + 1, r, pos, delta)
        return u

    def add(self, pos: int, delta: int) -> None:
        """插入位置 pos，个数增加 delta（delta 可为负表示删除）。"""
        self.root = self._add(self.root, self.min_value, self.max_value, pos, delta)

    def _query(self, u: int, l: int, r: int, ql: int, qr: int) -> int:
        """查询值域区间 [ql, qr] 的元素个数。"""
        if u == 0 or qr < l or r < ql:
            return 0
        if ql <= l and r <= qr:
            return self.tree[u].sum

        m = self.mid(l, r)
        return self._query(self.tree[u].left, l, m, ql, qr) + self._query(
            self.tree[u].right, m + 1, r, ql, qr
        )

    def query(self, ql: int, qr: int) -> int:
        return self._query(self.root, self.min_value, self.max_value, ql, qr)

    def count_less(self, x: int) -> int:
        """小于 x 的元素个数。"""
        if x <= self.min_value:
            return 0
        return self.query(self.min_value, x - 1)

    def count_leq(self, x: int) -> int:
        """小于等于 x 的元素个数。"""
        if x < self.min_value:
            return 0
        if x >= self.max_value:
            return self.tree[self.root].sum
        return self.query(self.min_value, x)

    def _kth(self, u: int, l: int, r: int, k: int) -> int:
        """第 k 小（k 从 1 开始）；k 越界时按 C++ 语义走到叶子返回该叶子值。"""
        if l == r:
            return l

        left_sum = self.tree[self.tree[u].left].sum if self.tree[u].left else 0
        m = self.mid(l, r)
        if k <= left_sum:
            return self._kth(self.tree[u].left, l, m, k)
        return self._kth(self.tree[u].right, m + 1, r, k - left_sum)

    def kth(self, k: int) -> int:
        return self._kth(self.root, self.min_value, self.max_value, k)

    def predecessor(self, x: int) -> int:
        """小于 x 的最大元素；不存在时按 C++ 语义返回 min_value。"""
        cnt = self.count_less(x)
        return self.kth(cnt)

    def successor(self, x: int) -> int:
        """大于 x 的最小元素；不存在时按 C++ 语义返回 max_value。"""
        cnt = self.count_leq(x)
        return self.kth(cnt + 1)

    def size(self) -> int:
        """元素总个数。"""
        return 0 if self.root == 0 else self.tree[self.root].sum
