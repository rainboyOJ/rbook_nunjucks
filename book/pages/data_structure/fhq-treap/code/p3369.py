import random
import sys

# 洛谷 P3369 【模板】普通平衡树
# 增加递归深度以应对最坏情况
sys.setrecursionlimit(200000)


class Node:
    __slots__ = ("val", "pri", "size", "l", "r")

    def __init__(self, val: int, pri: int):
        self.val = val
        self.pri = pri
        self.size = 1
        self.l = None
        self.r = None


class FHQTreap:
    def __init__(self, seed: int = 233):
        self.root = None
        self.rng = random.Random(seed)

    def _size(self, u):
        return u.size if u is not None else 0

    def _push_up(self, u):
        u.size = self._size(u.l) + self._size(u.r) + 1

    def _new_node(self, val: int):
        return Node(val, self.rng.randint(1, 2**31 - 1))

    def split(self, u, val: int):
        """按值分裂：<= val 归入左树，> val 归入右树"""
        if u is None:
            return None, None
        if u.val <= val:
            x, y = self.split(u.r, val)
            u.r = x
            self._push_up(u)
            return u, y
        else:
            x, y = self.split(u.l, val)
            u.l = y
            self._push_up(u)
            return x, u

    def merge(self, x, y):
        """合并两棵有序树（x 中最大值 <= y 中最小值）"""
        if x is None or y is None:
            return x if y is None else y
        if x.pri > y.pri:
            x.r = self.merge(x.r, y)
            self._push_up(x)
            return x
        else:
            y.l = self.merge(x, y.l)
            self._push_up(y)
            return y

    def insert(self, val: int):
        x, y = self.split(self.root, val)
        self.root = self.merge(self.merge(x, self._new_node(val)), y)

    def delete(self, val: int):
        x, z = self.split(self.root, val)
        x, y = self.split(x, val - 1)
        if y is not None:
            y = self.merge(y.l, y.r)
        self.root = self.merge(self.merge(x, y), z)

    def rank(self, val: int) -> int:
        u = self.root
        ans = 0
        while u is not None:
            if u.val < val:
                ans += self._size(u.l) + 1
                u = u.r
            else:
                u = u.l
        return ans + 1

    def kth(self, k: int) -> int:
        u = self.root
        while u is not None:
            l_sz = self._size(u.l)
            if k <= l_sz:
                u = u.l
            elif k == l_sz + 1:
                return u.val
            else:
                k -= l_sz + 1
                u = u.r
        return 0

    def pre(self, val: int) -> int:
        u = self.root
        ans = -2147483648
        while u is not None:
            if u.val < val:
                ans = u.val
                u = u.r
            else:
                u = u.l
        return ans

    def succ(self, val: int) -> int:
        u = self.root
        ans = 2147483647
        while u is not None:
            if u.val > val:
                ans = u.val
                u = u.l
            else:
                u = u.r
        return ans


def main():
    input_data = sys.stdin.read().split()
    if not input_data:
        return

    n = int(input_data[0])
    idx = 1
    treap = FHQTreap()
    output = []

    for _ in range(n):
        opt = int(input_data[idx])
        x = int(input_data[idx + 1])
        idx += 2

        if opt == 1:
            treap.insert(x)
        elif opt == 2:
            treap.delete(x)
        elif opt == 3:
            output.append(str(treap.rank(x)))
        elif opt == 4:
            output.append(str(treap.kth(x)))
        elif opt == 5:
            output.append(str(treap.pre(x)))
        elif opt == 6:
            output.append(str(treap.succ(x)))

    sys.stdout.write("\n".join(output) + "\n")


if __name__ == "__main__":
    main()
