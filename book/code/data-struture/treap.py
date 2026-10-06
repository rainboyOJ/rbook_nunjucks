# Treap 旋转平衡树：维护可重有序集合，支持插入/删除/排名/kth/前驱/后继。
# 节点 0 是空哨兵；不变量：child[0] 子树值 < value < child[1] 子树值，且
# priority 满足小根堆（父的 priority 小于孩子），期望树高 O(log n)。
# size = 左 size + 右 size + count，每次结构变化后都要 push_up。
# C++ 的 insert/erase/rotate 用 int& 引用出参回写子树根，Python 改为返回新根。
# insert/erase 是递归实现，随机优先级下期望深度 O(log n)，但最坏可退化；
# 若数据量达 1e5 量级，建议先 sys.setrecursionlimit(300000)。
# C++ 的 T 为 int，Python int 任意精度。

import random

INT_MIN: int = -(1 << 31)  # 对应 C++ INT_MIN，前驱无解时的返回值
INT_MAX: int = (1 << 31) - 1  # 对应 C++ INT_MAX，后继无解时的返回值

type Tree = list[Node]  # 节点池，下标即指针，下标 0 为空哨兵


class Node:
    """Treap 节点：child[0]/child[1] 为左右孩子。"""

    child: list[int]
    value: int
    priority: int  # 随机优先级（小根堆）
    count: int  # 相同值的个数
    size: int  # 子树节点总数（含 count）

    def __init__(self) -> None:
        self.child = [0, 0]
        self.value = 0
        self.priority = 0
        self.count = 0
        self.size = 0


class Treap:
    """旋转 Treap；max_nodes 仅为与 C++ 构造参数对齐，Python list 动态扩容故不使用。

    随机源与 C++ 的 mt19937(712367821) 不同，树形会不一样，
    但 rank_of/kth/predecessor/successor 的结果只依赖有序集合本身，与树形无关。
    """

    tree: Tree
    root: int
    rng: random.Random

    def __init__(self, max_nodes: int = 0) -> None:
        self.tree = [Node()]  # 0 号节点为空哨兵
        self.root = 0
        self.rng = random.Random(712367821)

    def new_node(self, value: int) -> int:
        """新建节点，返回其下标。"""
        self.tree.append(Node())
        node_id = len(self.tree) - 1
        self.tree[node_id].value = value
        # C++ 用 (int)rng() 取 32 位随机优先级，这里取 [0, 2^32) 的均匀随机数。
        self.tree[node_id].priority = self.rng.getrandbits(32)
        self.tree[node_id].count = 1
        self.tree[node_id].size = 1
        return node_id

    def node_size(self, u: int) -> int:
        """节点 u 的子树大小，空节点为 0。"""
        return 0 if u == 0 else self.tree[u].size

    def push_up(self, u: int) -> None:
        """上推：重算 u 的 size。"""
        self.tree[u].size = (
            self.node_size(self.tree[u].child[0])
            + self.node_size(self.tree[u].child[1])
            + self.tree[u].count
        )

    def rotate(self, u: int, direction: int) -> int:
        """旋转：direction=0 右旋提升左孩子，direction=1 左旋提升右孩子；返回新子树根。"""
        v = self.tree[u].child[direction]
        self.tree[u].child[direction] = self.tree[v].child[direction ^ 1]
        self.tree[v].child[direction ^ 1] = u
        self.push_up(u)
        self.push_up(v)
        return v

    def _insert(self, u: int, value: int) -> int:
        """在以 u 为根的子树中插入 value，返回新子树根。"""
        if u == 0:
            return self.new_node(value)
        if self.tree[u].value == value:
            self.tree[u].count += 1
            self.push_up(u)
            return u

        direction = 1 if value > self.tree[u].value else 0
        self.tree[u].child[direction] = self._insert(self.tree[u].child[direction], value)
        if self.tree[self.tree[u].child[direction]].priority < self.tree[u].priority:
            u = self.rotate(u, direction)
        self.push_up(u)
        return u

    def insert(self, value: int) -> None:
        self.root = self._insert(self.root, value)

    def _erase(self, u: int, value: int) -> int:
        """在以 u 为根的子树中删除一个 value，返回新子树根。"""
        if u == 0:
            return 0

        if self.tree[u].value == value:
            if self.tree[u].count > 1:
                self.tree[u].count -= 1
                self.push_up(u)
                return u

            left = self.tree[u].child[0]
            right = self.tree[u].child[1]
            if left == 0 or right == 0:
                return left + right  # 至多一个孩子，直接用它顶替 u

            # 把优先级更小的孩子旋上来，继续向下删，保持小根堆性质。
            direction = 0 if self.tree[left].priority < self.tree[right].priority else 1
            u = self.rotate(u, direction)
            self.tree[u].child[direction ^ 1] = self._erase(
                self.tree[u].child[direction ^ 1], value
            )
            self.push_up(u)
            return u

        direction = 1 if value > self.tree[u].value else 0
        self.tree[u].child[direction] = self._erase(self.tree[u].child[direction], value)
        self.push_up(u)
        return u

    def erase(self, value: int) -> None:
        self.root = self._erase(self.root, value)

    def rank_of(self, value: int) -> int:
        """排名（1-based）：最小的值排名 1。"""
        u = self.root
        rank = 1
        while u != 0:
            if value <= self.tree[u].value:
                u = self.tree[u].child[0]
            else:
                rank += self.node_size(self.tree[u].child[0]) + self.tree[u].count
                u = self.tree[u].child[1]
        return rank

    def kth(self, k: int) -> int:
        """第 k 小（k 从 1 开始）；越界时按 C++ 语义返回 -1。"""
        u = self.root
        while u != 0:
            left_size = self.node_size(self.tree[u].child[0])
            if k <= left_size:
                u = self.tree[u].child[0]
            elif k <= left_size + self.tree[u].count:
                return self.tree[u].value
            else:
                k -= left_size + self.tree[u].count
                u = self.tree[u].child[1]
        return -1

    def predecessor(self, value: int) -> int:
        """前驱：小于 value 的最大值；不存在时返回 INT_MIN。"""
        u = self.root
        answer = INT_MIN
        while u != 0:
            if self.tree[u].value < value:
                answer = self.tree[u].value
                u = self.tree[u].child[1]
            else:
                u = self.tree[u].child[0]
        return answer

    def successor(self, value: int) -> int:
        """后继：大于 value 的最小值；不存在时返回 INT_MAX。"""
        u = self.root
        answer = INT_MAX
        while u != 0:
            if self.tree[u].value > value:
                answer = self.tree[u].value
                u = self.tree[u].child[0]
            else:
                u = self.tree[u].child[1]
        return answer
