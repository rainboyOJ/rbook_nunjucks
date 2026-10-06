# BST 通用操作：左旋、右旋、找最小节点。
# 源 C++ 是模板类 BST_common_operation<Node, T> 的静态方法，操作 NodePtr&
# （指向子树根的指针的引用）；Python 没有引用传参，改为"引用槽" _Slot：
#   slot = _Slot(node)          # 整棵树的根槽
#   slot = Node.as_child(p, 1)  # p.left 槽（2 表示 p.right）
#   rotate_left(slot)           # 旋转直接写回槽位
# 旋转口诀:  1. 查空  2. 过继  3. 调整父子关系  4. 更新根
# 注意：旋转不改变 BST 中序序，只改变形态——这是旋转安全的前提。
# C++ 版不维护 parent 的场景（Node 未定义 isEmpty 时）由具体 Node 类决定；
# 这里按"维护 parent"的完整语义实现，与 rbtree.cpp 中的用法一致。

from __future__ import annotations


class Node:
    """最小可用节点：data、color 与三向指针。

    空节点约定：C++ 模板用 NIL 指针判空，这里用 None 表示空；
    若使用哨兵节点，把哨兵传入即可（isEmpty 语义由调用方保证）。
    """

    data: int
    color: int  # 颜色由使用方（如红黑树）自行约定，本文件不解释其含义
    left: Node | None
    right: Node | None
    parent: Node | None

    def __init__(self, data: int, color: int = 0) -> None:
        self.data = data
        self.color = color
        self.left = None
        self.right = None
        self.parent = None

    def is_empty(self) -> bool:
        """空节点判定：默认 None 才是空；哨兵节点子类可覆盖。"""
        return False


class _Slot:
    """模拟 C++ 的 NodePtr&：一个可读写的"根引用槽"。"""

    node: Node | None
    _parent: Node | None  # None 表示这是整棵树的根槽
    _which: int  # 0: 根槽（直接持有），1: 挂在 _parent.left，2: 挂在 _parent.right

    def __init__(self, node: Node | None) -> None:
        self.node = node
        self._parent = None
        self._which = 0

    @staticmethod
    def as_child(parent: Node, which: int) -> _Slot:
        s = _Slot(parent.left if which == 1 else parent.right)
        s._parent = parent
        s._which = which
        return s

    def _sync(self) -> None:
        # 把槽内指针写回父节点的对应位置
        if self._which == 1:
            self._parent.left = self.node
        elif self._which == 2:
            self._parent.right = self.node


def slot_of(root: Node, node: Node) -> _Slot:
    """构造挂在 node 位置的根槽（node 必须在以 root 为根的树中）。"""
    parent = node.parent
    if parent is None:
        return _Slot(node)
    if parent.left is node:
        return _Slot.as_child(parent, 1)
    return _Slot.as_child(parent, 2)


# 左旋，让右孩子 y 上位，自己 x 下沉
# 口诀:  1. 查空  2. 过继  3. 调整父子关系  4. 更新根
def rotate_left(x_slot: _Slot) -> None:
    x = x_slot.node
    if x is None or x.is_empty():
        return
    y = x.right
    if y is None or y.is_empty():
        return  # 节点或右孩子为空，无法左旋

    # 2. "过继" y 的左子树：y 上位后, y 原来的左子树 yl, 挂到 x 的右边
    x.right = y.left
    if y.left is not None and not y.left.is_empty():
        y.left.parent = x  # 更新 yl 的父节点

    # 3. x 连接到 y 的左边
    y.left = x
    y.parent = x.parent  # y 连接到 x 的原父节点 P
    x.parent = y

    # 4. 更新子树的根：由于传入的是引用槽, 写回它就是修改原先指向 x 的那个指针
    x_slot.node = y
    x_slot._sync()


# 右旋，让左孩子 x 上位，自己 y 下沉 (与左旋完全对称)
# 口诀:  1. 查空  2. 过继  3. 调整父子关系  4. 更新根
def rotate_right(y_slot: _Slot) -> None:
    y = y_slot.node
    if y is None or y.is_empty():
        return
    x = y.left
    if x is None or x.is_empty():
        return  # 节点或左孩子为空，无法右旋

    # 2. "过继" x 的右子树：x 上位后, x 原来的右子树 xr, 挂到 y 的左边
    y.left = x.right
    if x.right is not None and not x.right.is_empty():
        x.right.parent = y  # 更新 xr 的父节点

    # 3. y 连接到 x 的右边
    x.right = y
    x.parent = y.parent  # x 连接到 y 的原父节点 P
    y.parent = x

    # 4. 更新子树的根：由于传入的是引用槽, 写回它就是修改原先指向 y 的那个指针
    y_slot.node = x
    y_slot._sync()


def find_min(node: Node, nil: Node | None = None) -> Node:
    """Find the minimum value in the tree.

    :param node: The root of the tree.
    :param nil: The NIL node.
    :return: The minimum node in the tree.
    """
    while node.left is not nil:
        node = node.left
    return node
