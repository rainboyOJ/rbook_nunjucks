# FHQ-Treap（数组节点池版）：按值分裂/合并，随机优先级保持期望 O(log n)。
# tr[0] 是空哨兵，size/val 恒为 0，防止脏数据污染 size 计算；tr_idx 即节点池长度-1。
# 查询不存在时的返回值沿用 C++ 的哨兵：lower_bound/upper_bound 返回 INF_MAX，
# pre 返回 INF_MIN。Python int 无上下界，这里用 float('inf')/float('-inf') 表示，
# 与 int 比较安全（命中时返回的就是 int）。
# split/merge 递归深度为树高，期望 O(log n)；若担心退化可调高 sys.setrecursionlimit。

import random

type Found = int | float  # 查询结果：命中返回 int 值，未命中返回 ±inf 哨兵


class Node:
    """节点：l/r 为孩子下标（0 表示空），size 为子树大小，fix 为随机优先级，val 为值。"""

    __slots__ = ("l", "r", "size", "fix", "val")

    def __init__(self) -> None:
        self.l = 0
        self.r = 0
        self.size = 0
        self.fix = 0
        self.val = 0


class FHQ:
    """C++ 模板参数 N（定长数组上限）在 Python 里由 list 动态扩容取代，无需预分配。"""

    INF_MAX: float = float("inf")   # 找不到时 lower_bound/upper_bound 的返回值
    INF_MIN: float = float("-inf")  # 找不到时 pre 的返回值

    def __init__(self, seed: int = 233) -> None:
        self.rng = random.Random(seed)
        self.tr: list[Node] = [Node()]  # 0 号空哨兵
        self.root = 0
        self.init()

    # --- 1. 多组数据必备 (Clear & Init) ---

    def init(self) -> None:
        self.root = 0
        del self.tr[1:]  # 清空节点池，仅保留 0 号哨兵（对应 C++ tr_idx = 0）
        self.tr[0] = Node()

    def clear(self) -> None:
        self.init()

    def size(self) -> int:
        return self.tr[self.root].size

    def empty(self) -> bool:
        return self.size() == 0

    # --- 2. 核心操作 (内部使用) ---

    def new_node(self, v: int) -> int:
        node = Node()
        node.size = 1
        # 对应 C++ mt19937 的 32 位无符号输出，取 [0, 2^32) 均匀随机数。
        node.fix = self.rng.randrange(1 << 32)
        node.val = v
        self.tr.append(node)
        return len(self.tr) - 1

    def push_up(self, u: int) -> None:
        self.tr[u].size = self.tr[self.tr[u].l].size + self.tr[self.tr[u].r].size + 1

    def split(self, u: int, v: int) -> tuple[int, int]:
        """按值分裂：返回 (x, y)，x 含所有值 <= v 的节点，y 含所有值 > v 的节点。

        C++ 通过引用出参返回两棵树，Python 用元组返回，语义一致。
        """
        if u == 0:
            return 0, 0
        if self.tr[u].val <= v:
            x, y = self.split(self.tr[u].r, v)
            self.tr[u].r = x
            self.push_up(u)
            return u, y
        x, y = self.split(self.tr[u].l, v)
        self.tr[u].l = y
        self.push_up(u)
        return x, u

    def merge(self, x: int, y: int) -> int:
        """合并两棵树；前提是 x 中所有值 <= y 中所有值，按 fix 大根堆性质决定根。"""
        if x == 0 or y == 0:
            return x + y
        if self.tr[x].fix > self.tr[y].fix:
            self.tr[x].r = self.merge(self.tr[x].r, y)
            self.push_up(x)
            return x
        self.tr[y].l = self.merge(x, self.tr[y].l)
        self.push_up(y)
        return y

    # --- 3. 常用接口 (外部调用) ---

    def insert(self, v: int) -> None:
        x, y = self.split(self.root, v)
        self.root = self.merge(self.merge(x, self.new_node(v)), y)

    def erase(self, v: int) -> None:
        """删除一个值为 v 的节点（同值多个时只删一个）；v 不存在时静默无操作。"""
        x, z = self.split(self.root, v)
        x, y = self.split(x, v - 1)
        if y != 0:
            # 丢弃 y 的根节点，把它的左右子树合并回来。
            y = self.merge(self.tr[y].l, self.tr[y].r)
        self.root = self.merge(self.merge(x, y), z)

    def rank(self, v: int) -> int:
        """查询排名（比 v 小的数的个数 + 1）。"""
        u = self.root
        ans = 0
        while u != 0:
            if self.tr[u].val < v:
                ans += self.tr[self.tr[u].l].size + 1
                u = self.tr[u].r
            else:
                u = self.tr[u].l
        return ans + 1

    def kth(self, k: int) -> int:
        """查询第 k 小（1-based）。C++ 版不检查越界；Python 越界抛 IndexError 更安全。"""
        if not (1 <= k <= self.size()):
            raise IndexError(f"kth index {k} out of range (size={self.size()})")
        u = self.root
        while True:
            l_size = self.tr[self.tr[u].l].size
            if k <= l_size:
                u = self.tr[u].l
            elif k == l_size + 1:
                return self.tr[u].val
            else:
                k -= l_size + 1
                u = self.tr[u].r

    # --- 4. STL 风格查询接口 ---

    def lower_bound(self, v: int) -> Found:
        """第一个 >= v 的值；不存在返回 INF_MAX。"""
        u = self.root
        ans: Found = self.INF_MAX
        while u != 0:
            if self.tr[u].val >= v:
                ans = self.tr[u].val  # 记录可行解，再往左找更小的
                u = self.tr[u].l
            else:
                u = self.tr[u].r
        return ans

    def upper_bound(self, v: int) -> Found:
        """第一个 > v 的值；不存在返回 INF_MAX。"""
        u = self.root
        ans: Found = self.INF_MAX
        while u != 0:
            if self.tr[u].val > v:
                ans = self.tr[u].val
                u = self.tr[u].l
            else:
                u = self.tr[u].r
        return ans

    def pre(self, v: int) -> Found:
        """前驱，即 < v 的最大值；不存在返回 INF_MIN。"""
        u = self.root
        ans: Found = self.INF_MIN
        while u != 0:
            if self.tr[u].val < v:
                ans = self.tr[u].val  # 记录可行解，再往右找更大的
                u = self.tr[u].r
            else:
                u = self.tr[u].l
        return ans
