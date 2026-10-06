# bst 通用操作（rotateLeft / rotateRight / findMin）。
# 源 C++ 文件 bst_command2.cpp 是一份未完成的教学草稿（语法错误、半截代码），
# 无法编译运行；本文件移植其中逻辑完整、可独立成立的部分：
#   - bst_common.cpp 的三个通用操作（rotateLeft / rotateRight / findMin）
#   - bst_command2.cpp 草稿中的内存池 Mem 与 fhq（无旋 Treap）类骨架
# 旋转操作通过显式的"根引用槽"模拟 C++ 的 NodePtr&：child_slot 返回
# 一个可读写的引用对象，set_xxx 修改它就是修改父节点里的那个指针。
# 一行示例：slot = child_slot(root, node); rotate_left(slot)  ->  旋转后同槽位指向新子树根

from __future__ import annotations

from dataclasses import dataclass, field

# 旋转口诀: 1. 查空  2. 过继  3. 调整父子关系  4. 更新根


class _Slot:
    """模拟 C++ 的 Node*&：一个可读写的"根引用槽"。

    根槽的 node 可直接赋值，赋值即替换父节点中的对应指针。
    """

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


def child_slot(root: Node, node: Node) -> _Slot:
    """构造挂在 node 位置的根槽（node 必须在以 root 为根的树中）。"""
    parent = node.parent
    if parent is None:
        return _Slot(node)
    if parent.left is node:
        return _Slot.as_child(parent, 1)
    return _Slot.as_child(parent, 2)


def rotate_left(x_slot: _Slot) -> None:
    """左旋，让右孩子 y 上位，自己 x 下沉。

    通过 x_slot 修改"指向 x 的那个指针"，对应 C++ 的 NodePtr& x。
    """
    x = x_slot.node
    if x is None:
        return
    y = x.right
    if y is None:
        return  # 节点或右孩子为空，无法左旋

    # 2. "过继" y 的左子树：挂到 x 的右边
    x.right = y.left
    if y.left is not None:
        y.left.parent = x  # 更新 yl 的父节点

    # 3. x 连接到 y 的左边
    y.left = x
    y.parent = x.parent  # y 连接到 x 的原父节点 P
    x.parent = y

    # 4. 更新子树的根：写回槽位
    x_slot.node = y
    x_slot._sync()


def rotate_right(y_slot: _Slot) -> None:
    """右旋，让左孩子 x 上位，自己 y 下沉（与左旋完全对称）。"""
    y = y_slot.node
    if y is None:
        return
    x = y.left
    if x is None:
        return  # 节点或左孩子为空，无法右旋

    # 2. "过继" x 的右子树：挂到 y 的左边
    y.left = x.right
    if x.right is not None:
        x.right.parent = y  # 更新 xr 的父节点

    # 3. y 连接到 x 的右边
    x.right = y
    x.parent = y.parent  # x 连接到 y 的原父节点 P
    y.parent = x

    # 4. 更新子树的根：写回槽位
    y_slot.node = x
    y_slot._sync()


def find_min(node: Node, nil: Node | None = None) -> Node:
    """找 node 子树的最小节点；node 为空节点（等于 nil）时行为未定义，
    与 C++ 一致——调用方保证传入非空子树根。
    """
    while node.left is not nil:
        node = node.left
    return node


# ---- 以下为 bst_command2.cpp 草稿中可成立的骨架 ----


@dataclass
class Mem:
    """定长内存池：按序分配下标。

    C++ 草稿里 Mem::get 只 ++idx（少写了 return，属于笔误），
    这里按"分配一个新下标"的语义补全。
    """

    size: int
    mem: list[object] = field(default_factory=list)  # mem[i] 为第 i 个槽位存的对象
    idx: int = 0  # 已分配槽位数（下一个分配出去的下标）

    def get(self) -> int:
        """分配一个槽位下标。"""
        self.idx += 1
        return self.idx


@dataclass
class Node:
    """bst_command2.cpp 草稿中的 treap 节点结构（字段与 C++ 一致）。"""

    l: int  # 左子结点下标
    r: int  # 右子结点下标
    val: int
    fix: int  # treap 随机优先级
    size: int  # 子树大小


@dataclass
class BstNodeComm:
    """bst_command2.cpp 中 bst_node_comm 类骨架：只保留能成立的成员布局。

    rotateLeft / rotateRight / rank 在草稿里均为半截代码，未定义行为，
    故不在此虚构实现；Mem 池与左右子结点/父亲下标的布局照搬。
    """

    mem: Mem
    l: int = 0  # 左子结点下标
    r: int = 0  # 右子结点下标
    fa: int = 0  # 记录父亲

    def get(self) -> int:
        return self.mem.get()
