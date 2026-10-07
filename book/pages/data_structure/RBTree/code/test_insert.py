# 红黑树随机插入测试，对应同目录 test_insert.cpp。
# C++ 版用 std::random_device 给 mt19937 播种，输出每次运行都不同，
# 因此 Python 版同样不固定种子（random.Random() 由操作系统熵播种），
# 只保证格式与语义一致，无法与 C++ 逐字节对拍。
# 不读输入；共跑 20 轮，每轮插入 n 个 [0, 99] 的随机值并校验红黑性质。
# ins 递归深度 = 树高 <= 2*log2(n+1)，n < 100 时约 14，无需调 sys.setrecursionlimit。

from __future__ import annotations

import pathlib
import random
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


def random_test(rng: random.Random) -> None:
    tree = rbtree.RBTree()
    n = rng.randrange(100)  # 对应 C++ 的 rand() % 100，取值 [0, 99]
    print(f"insert n = {n}")
    print(">>> Insert values : ", end="")
    values: list[int] = []
    for _ in range(n):
        val = rng.randrange(100)
        print(f"{val} ", end="")
        values.append(val)
    print()

    for val in values:
        tree.insert(val)

    print("Validation: " + ("Valid" if tree.is_valid() else "Invalid"))
    print()
    print("-----------------------------------")


def main() -> None:
    print("Inserting values into the RBTree...")
    print("-----------------------------------")

    rng = random.Random()  # 对应 std::mt19937 rng(std::random_device{}())
    for i in range(20):
        print(f"test i = {i + 1} | ", end="")
        random_test(rng)


if __name__ == "__main__":
    main()
