# AVL 树演示：插入 + 删除 + 中序遍历，对应同目录 demo.cpp。
# 程序本身不读输入，按固定序列跑一遍并打印结果。
# 不变量：h 为子树高度，空节点高度 0；每个节点左右子树高度差不超过 1。
# 递归深度 = 树高 O(log n)，这里只有几个节点，无需调 sys.setrecursionlimit。

from __future__ import annotations


class Node:
    """val 是键；h 是子树高度，叶子节点高度为 1。"""

    def __init__(self, val: int) -> None:
        self.val = val
        self.h = 1
        self.left: Node | None = None
        self.right: Node | None = None


def get_height(node: Node | None) -> int:
    # 空节点高度为 0，作为所有高度公式的基准。
    if node is None:
        return 0
    return node.h


def get_balance_factor(node: Node | None) -> int:
    # 左子树高度 - 右子树高度：> 1 左边太高，< -1 右边太高。
    if node is None:
        return 0
    return get_height(node.left) - get_height(node.right)


def update_height(node: Node | None) -> None:
    if node is not None:
        node.h = max(get_height(node.left), get_height(node.right)) + 1


def rotate_right(y: Node) -> Node:
    """右旋：左孩子 x 上位，y 下沉。适用左左 (LL) 失衡。"""
    x = y.left
    t2 = x.right
    x.right = y
    y.left = t2
    # 必须先更新子节点 y，再更新父节点 x，否则读到的是旧高度。
    update_height(y)
    update_height(x)
    return x


def rotate_left(x: Node) -> Node:
    """左旋：右孩子 y 上位，x 下沉。适用右右 (RR) 失衡。"""
    y = x.right
    t2 = y.left
    y.left = x
    x.right = t2
    # 先子后父。
    update_height(x)
    update_height(y)
    return y


def rebalance(node: Node) -> Node:
    """插入/删除回溯时调用：更新高度，必要时旋转，返回新的子树根。"""
    update_height(node)
    balance = get_balance_factor(node)

    if balance > 1:
        if get_balance_factor(node.left) >= 0:
            return rotate_right(node)  # LL
        node.left = rotate_left(node.left)  # LR
        return rotate_right(node)

    if balance < -1:
        if get_balance_factor(node.right) <= 0:
            return rotate_left(node)  # RR
        node.right = rotate_right(node.right)  # RL
        return rotate_left(node)

    return node


def insert(node: Node | None, val: int) -> Node:
    """递归插入；重复值直接忽略（与 C++ 的 else 分支一致）。"""
    if node is None:
        return Node(val)
    if val < node.val:
        node.left = insert(node.left, val)
    elif val > node.val:
        node.right = insert(node.right, val)
    else:
        return node
    return rebalance(node)


def get_min_value_node(node: Node) -> Node:
    """中序后继：右子树里一路向左。"""
    current = node
    while current.left is not None:
        current = current.left
    return current


def remove(root: Node | None, val: int) -> Node | None:
    if root is None:
        return root

    if val < root.val:
        root.left = remove(root.left, val)
    elif val > root.val:
        root.right = remove(root.right, val)
    else:
        if root.left is None or root.right is None:
            # 0 或 1 个孩子：用唯一的孩子（可能为空）顶替自己。
            # C++ 里是 *root = *temp 浅拷贝；Python 直接返回孩子即可。
            return root.left if root.left is not None else root.right
        # 2 个孩子：把右子树最小值复制上来，再递归删掉那个后继。
        temp = get_min_value_node(root.right)
        root.val = temp.val
        root.right = remove(root.right, temp.val)

    if root is None:
        return root
    return rebalance(root)


def in_order(root: Node | None, out: list[str]) -> None:
    # C++ 是边遍历边 cout，这里收集成列表；末尾的空格由调用方补齐。
    if root is not None:
        in_order(root.left, out)
        out.append(str(root.val))
        in_order(root.right, out)


def main() -> None:
    root: Node | None = None

    print("插入: 10, 20, 30, 40, 50, 25")
    for val in (10, 20, 30, 40, 50, 25):  # 30 触发 RR，25 触发 RL
        root = insert(root, val)

    order: list[str] = []
    in_order(root, order)
    # C++ 每个值后面都跟一个空格，行尾同样有空格。
    print("中序遍历 (应该是排序的): " + "".join(v + " " for v in order))
    print(f"根节点是: {root.val} (树是平衡的)")

    print()
    print("删除: 30")
    root = remove(root, 30)

    order = []
    in_order(root, order)
    print("中序遍历: " + "".join(v + " " for v in order))
    print(f"当前根节点: {root.val}")


if __name__ == "__main__":
    main()
