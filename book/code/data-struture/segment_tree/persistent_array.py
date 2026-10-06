# 可持久化数组：每次 update 只复制一条到根的路径，产生新版本，旧版本仍可查询。
# 节点用下标池存储，0 号为空节点；根下标即版本句柄（对应 C++ 的 root[版本]）。
# build / update / query 递归深度均为线段树高度 O(log n)，不会触及默认递归上限。
# C++ 用 long long 存值，Python int 任意精度，无溢出问题。


class Node:
    """线段树节点：left/right 为孩子下标（0 表示空），value 为单点值。"""

    __slots__ = ("left", "right", "value")

    def __init__(self) -> None:
        self.left = 0
        self.right = 0
        self.value = 0


class PersistentArray:
    def __init__(self, max_nodes: int) -> None:
        # C++ 的 max_nodes 用于 reserve 预分配；Python list 动态扩容，该参数仅为
        # 保持接口一致而保留，不参与逻辑（传任意非负整数都不影响结果）。
        self.tree: list[Node] = [Node()]  # 0 号节点为空节点

    @staticmethod
    def mid(l: int, r: int) -> int:
        return (l + r) >> 1

    def clone(self, p: int) -> int:
        """复制节点 p 并返回新节点下标（浅拷贝 l/r/value，与 C++ 逐字段复制一致）。"""
        src = self.tree[p]
        node = Node()
        node.left, node.right, node.value = src.left, src.right, src.value
        self.tree.append(node)
        return len(self.tree) - 1

    def build(self, l: int, r: int, a: list[int]) -> int:
        """用 1 下标数组 a 的 [l, r] 建树，返回根下标。"""
        p = self.clone(0)
        if l == r:
            self.tree[p].value = a[l]
            return p
        m = self.mid(l, r)
        self.tree[p].left = self.build(l, m, a)
        self.tree[p].right = self.build(m + 1, r, a)
        return p

    def update(self, p: int, l: int, r: int, pos: int, value: int) -> int:
        """基于版本 p 把位置 pos 改成 value，返回新版本根下标（不改动原版本）。"""
        q = self.clone(p)
        if l == r:
            self.tree[q].value = value
            return q
        m = self.mid(l, r)
        if pos <= m:
            self.tree[q].left = self.update(self.tree[p].left, l, m, pos, value)
        else:
            self.tree[q].right = self.update(self.tree[p].right, m + 1, r, pos, value)
        return q

    def query(self, p: int, l: int, r: int, pos: int) -> int:
        """查询版本 p 中位置 pos 的值。"""
        if l == r:
            return self.tree[p].value
        m = self.mid(l, r)
        if pos <= m:
            return self.query(self.tree[p].left, l, m, pos)
        return self.query(self.tree[p].right, m + 1, r, pos)
