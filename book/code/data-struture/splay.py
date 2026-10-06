# Splay 伸展树：维护可重有序集合，支持插入/删除/排名/kth/前驱/后继，均摊 O(log n)。
# 节点 0 是空哨兵（child = {0,0}, parent = 0, count = size = 0），root 指向树根。
# 不变量：每个节点 child[0] 子树的值都 < value，child[1] 子树的值都 > value；
# size = 左 size + 右 size + count，每次结构变化后都要 push_up。
# 全部操作迭代实现，无需担心递归深度。C++ 的 T 为 int，Python int 任意精度。

INT_MIN: int = -(1 << 31)  # 对应 C++ INT_MIN，前驱无解时的返回值
INT_MAX: int = (1 << 31) - 1  # 对应 C++ INT_MAX，后继无解时的返回值

type Tree = list[Node]  # 节点池，下标即指针，下标 0 为空哨兵


class Node:
    """伸展树节点：child[0]/child[1] 为左右孩子，parent 为父节点。"""

    child: list[int]
    parent: int
    value: int
    count: int  # 相同值的个数
    size: int  # 子树节点总数（含 count）

    def __init__(self) -> None:
        self.child = [0, 0]
        self.parent = 0
        self.value = 0
        self.count = 0
        self.size = 0


class Splay:
    """伸展树；max_nodes 仅为与 C++ 构造参数对齐，Python list 动态扩容故不使用。"""

    tree: Tree
    root: int

    def __init__(self, max_nodes: int = 0) -> None:
        self.tree = [Node()]  # 0 号节点为空哨兵
        self.root = 0

    def node_size(self, u: int) -> int:
        """节点 u 的子树大小，空节点为 0。"""
        return 0 if u == 0 else self.tree[u].size

    def push_up(self, u: int) -> None:
        """上推：重算 u 的 size。"""
        if u == 0:
            return
        self.tree[u].size = (
            self.node_size(self.tree[u].child[0])
            + self.node_size(self.tree[u].child[1])
            + self.tree[u].count
        )

    def new_node(self, value: int, parent: int) -> int:
        """新建节点，返回其下标。"""
        self.tree.append(Node())
        node_id = len(self.tree) - 1
        self.tree[node_id].value = value
        self.tree[node_id].count = 1
        self.tree[node_id].size = 1
        self.tree[node_id].parent = parent
        return node_id

    def direction(self, u: int) -> int:
        """u 是父节点的哪个孩子（0 左 1 右）。"""
        p = self.tree[u].parent
        return 1 if self.tree[p].child[1] == u else 0

    def connect(self, child: int, parent: int, dir: int) -> None:
        """连接：child 作为 parent 的 dir 方向孩子。"""
        if parent != 0:
            self.tree[parent].child[dir] = child
        if child != 0:
            self.tree[child].parent = parent

    def rotate(self, x: int) -> None:
        """旋转 x 上移一层。"""
        y = self.tree[x].parent
        z = self.tree[y].parent
        dx = self.direction(x)
        dy = 0 if z == 0 else self.direction(y)
        middle = self.tree[x].child[dx ^ 1]  # dx 的另一侧：dx=0 取右孩子，dx=1 取左孩子

        self.connect(middle, y, dx)
        self.connect(y, x, dx ^ 1)
        self.connect(x, z, dy)

        self.push_up(y)
        self.push_up(x)
        if z == 0:
            self.root = x

    def splay(self, x: int, goal: int = 0) -> None:
        """把 x 伸展到 goal 的孩子（goal = 0 表示伸展到根）。"""
        if x == 0:
            return
        while self.tree[x].parent != goal:
            y = self.tree[x].parent
            z = self.tree[y].parent
            if z != goal:
                # 同向先转父节点（zig-zig），异向先转 x（zig-zag），否则会退化。
                if self.direction(x) == self.direction(y):
                    self.rotate(y)
                else:
                    self.rotate(x)
            self.rotate(x)
        if goal == 0:
            self.root = x

    def find(self, value: int) -> int:
        """查找值 value，找到则伸展到根并返回下标，否则返回 0。"""
        u = self.root
        last = 0
        while u != 0:
            last = u
            if value == self.tree[u].value:
                self.splay(u)
                return u
            u = self.tree[u].child[1 if value > self.tree[last].value else 0]
        if last != 0:
            self.splay(last)
        return 0

    def insert(self, value: int) -> None:
        """插入值 value。"""
        if self.root == 0:
            self.root = self.new_node(value, 0)
            return

        u = self.root
        parent = 0
        while u != 0:
            parent = u
            if value == self.tree[u].value:
                self.tree[u].count += 1
                self.push_up(u)
                self.splay(u)
                return
            u = self.tree[u].child[1 if value > self.tree[u].value else 0]

        dir = 1 if value > self.tree[parent].value else 0
        node_id = self.new_node(value, parent)
        self.tree[parent].child[dir] = node_id
        self.push_up(parent)
        self.splay(node_id)

    def erase(self, value: int) -> None:
        """删除一个值 value（若存在多个相同值只删一个）。"""
        target = self.find(value)
        if target == 0 or self.tree[target].value != value:
            return

        if self.tree[target].count > 1:
            self.tree[target].count -= 1
            self.push_up(target)
            return

        left = self.tree[target].child[0]
        right = self.tree[target].child[1]

        if left == 0:
            self.root = right
            if self.root != 0:
                self.tree[self.root].parent = 0
            return
        if right == 0:
            self.root = left
            self.tree[self.root].parent = 0
            return

        self.tree[left].parent = 0
        self.tree[right].parent = 0
        self.root = left

        # 把左子树的最大值转到根，再把右子树挂到它右边，即得删除后的合法 BST。
        u = left
        while self.tree[u].child[1] != 0:
            u = self.tree[u].child[1]
        self.splay(u)

        self.tree[self.root].child[1] = right
        self.tree[right].parent = self.root
        self.push_up(self.root)

    def rank_of(self, value: int) -> int:
        """排名（1-based）：最小的值排名 1。"""
        u = self.root
        last = 0
        rank = 1
        while u != 0:
            last = u
            if value <= self.tree[u].value:
                u = self.tree[u].child[0]
            else:
                rank += self.node_size(self.tree[u].child[0]) + self.tree[u].count
                u = self.tree[u].child[1]
        if last != 0:
            self.splay(last)
        return rank

    def kth(self, k: int) -> int:
        """第 k 小（k 从 1 开始）；越界时按 C++ 语义返回 -1。"""
        u = self.root
        while u != 0:
            left_size = self.node_size(self.tree[u].child[0])
            if k <= left_size:
                u = self.tree[u].child[0]
            elif k <= left_size + self.tree[u].count:
                self.splay(u)
                return self.tree[u].value
            else:
                k -= left_size + self.tree[u].count
                u = self.tree[u].child[1]
        return -1

    def predecessor(self, value: int) -> int:
        """前驱：小于 value 的最大值；不存在时返回 INT_MIN。"""
        u = self.root
        best = 0
        answer = INT_MIN
        while u != 0:
            if self.tree[u].value < value:
                best = u
                answer = self.tree[u].value
                u = self.tree[u].child[1]
            else:
                u = self.tree[u].child[0]
        if best != 0:
            self.splay(best)
        return answer

    def successor(self, value: int) -> int:
        """后继：大于 value 的最小值；不存在时返回 INT_MAX。"""
        u = self.root
        best = 0
        answer = INT_MAX
        while u != 0:
            if self.tree[u].value > value:
                best = u
                answer = self.tree[u].value
                u = self.tree[u].child[0]
            else:
                u = self.tree[u].child[1]
        if best != 0:
            self.splay(best)
        return answer
