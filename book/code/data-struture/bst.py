# 普通二叉搜索树（不旋转、不平衡），键为 int。
# BST 不变量：任意节点左子树所有键 < 该节点键 < 右子树所有键；键不重复。
# C++ 版大量使用 Node*&（引用出参）修改子树根，Python 没有引用传参，
# 对应函数改为"传入根、返回新根"，调用方需写 root = insert(x, root)。
# 一行示例：root = insert(3, insert(1, None)); find(3, root)  ->  True
# insert_dfs / find 是递归实现，树最坏退化成链（有序插入），
# 深度可达 O(n)，n 在 1e5 量级时注意 sys.setrecursionlimit 或改用 _iterative 版本。
# C++ 版的 printTree 依赖标准输出，按本仓 Python 模板规范（无 I/O）未移植。

from __future__ import annotations


class Node:
    """BST 节点：key 为键，left/right/parent 为子与父指针（None 表示空）。"""

    key: int
    left: Node | None
    right: Node | None
    parent: Node | None

    def __init__(self, key: int) -> None:
        self.key = key
        self.left = None
        self.right = None
        self.parent = None


# 查找操作（递归实现）
def find(x: int, t: Node | None) -> bool:
    if t is None:
        return False
    if x < t.key:
        return find(x, t.left)
    elif x > t.key:
        return find(x, t.right)
    else:
        return True


# 查找操作（非递归实现）
def find_iterative(x: int, t: Node | None) -> bool:
    while t is not None:
        if x < t.key:
            t = t.left
        elif x > t.key:
            t = t.right
        else:
            return True
    return False


# 插入操作（递归实现），返回插入后的子树根。
# 注意：这个实现正确维护 parent 指针（C++ 注释说没处理，Python 版顺手补上，
# 语义与推荐的非递归版本一致）。
def insert_dfs(x: int, t: Node | None) -> Node:
    if t is None:
        return Node(x)
    if x < t.key:
        t.left = insert_dfs(x, t.left)
        if t.left is not None:
            t.left.parent = t  # 设置父节点
    elif x > t.key:
        t.right = insert_dfs(x, t.right)
        if t.right is not None:
            t.right.parent = t  # 设置父节点
    # 如果 x == t.key，不做任何操作（键不重复）
    return t


# 插入操作（非递归实现，推荐），返回插入后的根。
def insert(x: int, t: Node | None) -> Node:
    p = t
    parent: Node | None = None
    while p is not None:
        parent = p  # 记录父节点
        if x < p.key:
            p = p.left
        elif x > p.key:
            p = p.right
        else:
            return t  # 元素已存在，树不变
    new_node = Node(x)
    new_node.parent = parent  # 设置新节点的父节点
    if parent is None:
        return new_node  # 树为空，新节点为根
    elif x < parent.key:  # 根据 BST 不变量决定插入位置
        parent.left = new_node
    else:
        parent.right = new_node
    return t


# 查找最小节点（返回指针，空树返回 None）
def find_min_node(t: Node | None) -> Node | None:
    if t is None:
        return None
    while t.left is not None:
        t = t.left
    return t


# 查找最大节点（返回指针，空树返回 None）
def find_max_node(t: Node | None) -> Node | None:
    if t is None:
        return None
    while t.right is not None:
        t = t.right
    return t


# 查找最小值（返回键值，空树返回 None，对应 std::optional）
def find_min(t: Node | None) -> int | None:
    min_node = find_min_node(t)
    if min_node is not None:
        return min_node.key
    return None


# 查找最大值（返回键值，空树返回 None，对应 std::optional）
def find_max(t: Node | None) -> int | None:
    max_node = find_max_node(t)
    if max_node is not None:
        return max_node.key
    return None


# 查找后继（中序意义下的下一个节点，x 为最大值时返回 None）
def succ(x: Node | None) -> Node | None:
    if x is None:
        return None

    # 情况 1: 节点有右子树，后继是右子树的最小节点
    if x.right is not None:
        return find_min_node(x.right)

    # 情况 2: 节点没有右子树，沿父指针向上走到"从右侧离开"的那一步
    p = x.parent
    while p is not None and x is p.right:
        x = p
        p = p.parent
    return p  # p 是后继, 或者 p 是 None（x 是最大值）


# 查找前驱（中序意义下的上一个节点，x 为最小值时返回 None）
def prev(x: Node | None) -> Node | None:
    if x is None:
        return None

    # 情况 1: 节点有左子树，前驱是左子树的最大节点
    if x.left is not None:
        return find_max_node(x.left)

    # 情况 2: 节点没有左子树，沿父指针向上走到"从左侧离开"的那一步
    p = x.parent
    while p is not None and x is p.left:
        x = p
        p = p.parent
    return p  # p 是前驱, 或者 p 是 None（x 是最小值）


# 删除最小值，返回删除后的根。
def delete_min(root: Node | None) -> Node | None:
    if root is None:
        return None

    min_node = find_min_node(root)
    if min_node is None:
        return root

    # 最小节点没有左孩子，直接用右孩子顶替
    if min_node.parent is None:  # 是根节点
        root = min_node.right
    else:
        min_node.parent.left = min_node.right

    if min_node.right is not None:
        min_node.right.parent = min_node.parent

    return root


# 删除键为 x 的节点（不存在则原样返回），返回删除后的根。
def delete_node(x: int, root: Node | None) -> Node | None:
    if root is None:
        return None

    current = root
    # 1. 先找到要删除的节点
    while current is not None and current.key != x:
        if x < current.key:
            current = current.left
        else:
            current = current.right

    if current is None:
        return root  # 没找到要删除的节点

    p = current.parent

    # 2. 处理删除的三种情况
    # 情况 1: 删除叶子节点
    if current.left is None and current.right is None:
        if p is None:  # 删除的是根节点
            root = None
        elif p.left is current:
            p.left = None
        else:
            p.right = None
        return root

    # 情况 2: 删除只有一个孩子的节点
    if current.left is None or current.right is None:
        child = current.left if current.left is not None else current.right

        if p is None:  # 删除的是根节点
            root = child
            child.parent = None
        elif p.left is current:
            p.left = child
            child.parent = p
        else:
            p.right = child
            child.parent = p
        return root

    # 情况 3: 删除有两个孩子的节点
    # 找到后继节点（右子树的最小值），用它的值替换当前节点的值
    successor = find_min_node(current.right)
    # 后继必然存在：右子树非空
    assert successor is not None
    current.key = successor.key

    # 删除后继节点（后继节点最多只有一个右孩子）
    # 注意：后继节点的父节点不可能是 current（current 有左孩子）
    if successor.parent is not None and successor.parent.left is successor:
        successor.parent.left = successor.right
    elif successor.parent is not None:
        successor.parent.right = successor.right

    if successor.right is not None:
        successor.right.parent = successor.parent

    return root


# 辅助函数：断开所有节点间的引用。C++ 需要 freeTree 释放内存，
# Python 靠 GC 自动回收，这里保留同名函数仅清空引用，方便复用对象池场景。
def free_tree(node: Node | None) -> None:
    if node is None:
        return
    free_tree(node.left)
    free_tree(node.right)
    node.left = None
    node.right = None
    node.parent = None


# 辅助函数：中序遍历，用于验证（结果为升序序列）
def in_order_traversal(node: Node | None, result: list[int]) -> None:
    if node is None:
        return
    in_order_traversal(node.left, result)
    result.append(node.key)
    in_order_traversal(node.right, result)
