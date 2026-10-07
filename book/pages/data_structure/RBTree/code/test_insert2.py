# 红黑树插入测试（带调试输出），对应同目录 test_insert2.cpp。
# C++ 版用 #define RBTree_DEBUG 后 #include "rbtree.cpp"，会额外打印每次
# balance() 命中的模式和整棵树；这里同样复用仓库模板 rbtree.py，
# 只重写 balance() 补上 C++ 的 " match: i node: X -> 模式" 调试行。
# 不读输入，按固定序列 2,6,5,7,0,1,8,4,3 插入并逐次打印结构 + 校验结果。

from __future__ import annotations

import pathlib
import sys


def _load_rbtree_module():
    """定位并导入 book/code/data-struture/RBTree/rbtree.py（等价于 C++ 的 #include "rbtree.cpp"）。"""
    for parent in pathlib.Path(__file__).resolve().parents:
        candidate = parent / "book" / "code" / "data-struture" / "RBTree"
        if (candidate / "rbtree.py").is_file():
            sys.path.insert(0, str(candidate))
            import rbtree  # noqa: E402  （必须先补 sys.path 才能导入）

            return rbtree
    raise FileNotFoundError("未找到 book/code/data-struture/RBTree/rbtree.py")


rbtree = _load_rbtree_module()
# 对应 C++ 的 #define RBTree_DEBUG：打开调试输出。
rbtree.enable_debug()


class DebugRBTree(rbtree.RBTree):
    """模板的 balance() 没有调试输出，这里补上命中模式那一行。

    C++ 的打印顺序是「先打 match 行，再执行 operate」，所以要在调用父类前打印。
    """

    def balance(self, node_ref: rbtree._Ref) -> rbtree.Node:
        node = node_ref.node
        for i, pattern in enumerate(rbtree._INSERT_PATTERNS):
            if pattern.match(node):
                print(f" match: {i} node: {node.data} -> ", end="")
                pattern.debug()
                break
        return super().balance(node_ref)


def main() -> None:
    tree = DebugRBTree()

    print("Inserting values into the RBTree...")
    print("-----------------------------------")

    for val in (2, 6, 5, 7, 0, 1, 8, 4, 3):
        print(f">>> Inserting {val}...")
        tree.insert(val)
        tree.print()
        print("Validation: " + ("Valid" if tree.is_valid() else "Invalid"))
        print()
        print("-----------------------------------")

    print("Final RBTree structure:")
    tree.print()
    print()
    print("Validation: " + ("Valid" if tree.is_valid() else "Invalid"))


if __name__ == "__main__":
    main()
