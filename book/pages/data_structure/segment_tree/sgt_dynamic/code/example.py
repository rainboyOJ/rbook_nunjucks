# 动态开点线段树示例，对应同目录 example.cpp。
# 程序不读输入：在值域 [1, 1e9] 上做三次单点加，再查询两个区间。
# 不变量：下标 0 表示空节点（C++ 用 tot = 0 当 null），节点从 1 开始编号。
# 递归深度 = O(log V) ≈ 30，无需调 sys.setrecursionlimit。
# C++ 版把 3 个 3e6 长的数组当结构体成员，约 60 MB 落在栈上会爆栈；
# Python 版改用 list 动态增长，只按需分配，不会爆内存。

import sys


class DynamicSegTree:
    """区间和动态开点线段树：只在被修改的路径上建节点。"""

    def __init__(self) -> None:
        self.ls = [0]  # ls[i]：节点 i 的左孩子编号，0 表示不存在
        self.rs = [0]  # rs[i]：节点 i 的右孩子编号
        self.sum = [0]  # sum[i]：节点 i 维护的区间和
        self.root = 0
        self.tot = 0

    def pushup(self, p: int) -> None:
        self.sum[p] = self.sum[self.ls[p]] + self.sum[self.rs[p]]

    def update(self, p: int, l: int, r: int, x: int, val: int) -> int:
        """单点加 a[x] += val，返回该子树（可能新建）的根编号。

        C++ 用 int& 出参直接改指针，Python 只能把新编号返回给调用方写回。
        """
        if p == 0:
            self.tot += 1
            p = self.tot
            self.ls.append(0)
            self.rs.append(0)
            self.sum.append(0)
        if l == r:
            self.sum[p] += val
            return p
        # 写成 l + ((r - l) >> 1) 而不是 (l + r) / 2，避免值域很大时 l + r 溢出（Python 无此问题，保持同构）。
        mid = l + ((r - l) >> 1)
        if x <= mid:
            self.ls[p] = self.update(self.ls[p], l, mid, x, val)
        else:
            self.rs[p] = self.update(self.rs[p], mid + 1, r, x, val)
        self.pushup(p)
        return p

    def query(self, p: int, l: int, r: int, ql: int, qr: int) -> int:
        """区间 [ql, qr] 求和；没建过的子树贡献 0。"""
        if p == 0:
            return 0
        if ql <= l and r <= qr:
            return self.sum[p]
        mid = l + ((r - l) >> 1)
        res = 0
        if ql <= mid:
            res += self.query(self.ls[p], l, mid, ql, qr)
        if qr > mid:
            res += self.query(self.rs[p], mid + 1, r, ql, qr)
        return res


def main() -> None:
    tree = DynamicSegTree()
    limit = 10**9

    tree.root = tree.update(tree.root, 1, limit, 1000, 5)
    tree.root = tree.update(tree.root, 1, limit, 2000000, 10)
    tree.root = tree.update(tree.root, 1, limit, 999999999, 20)

    # [1, 10000] 只覆盖 1000 这个修改点。
    sys.stdout.write(str(tree.query(tree.root, 1, limit, 1, 10000)) + "\n")
    # [1, 1e9] 覆盖全部三个修改点。
    sys.stdout.write(str(tree.query(tree.root, 1, limit, 1, limit)) + "\n")
    sys.stdout.write(f"Allocated nodes: {tree.tot}\n")


if __name__ == "__main__":
    main()
