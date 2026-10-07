# 洛谷 P3374 树状数组 1：单点加 + 区间和。
# 下标从 1 开始，lowbit(0) = 0 会让修改循环原地打转。
# C++ 用 long long 防溢出；Python int 是任意精度，不必特判。

import sys


class Fenwick:
    """tree[i] 维护 a[i - lowbit(i) + 1 .. i] 的元素和。"""

    def __init__(self, size: int = 0) -> None:
        self.n = size
        self.tree = [0] * (size + 1)

    @staticmethod
    def lowbit(x: int) -> int:
        # 最低位那个 1 代表的块长，即 x & -x。
        return x & -x

    def add(self, pos: int, value: int) -> None:
        i = pos
        while i <= self.n:
            self.tree[i] += value
            i += self.lowbit(i)

    def prefix_sum(self, pos: int) -> int:
        answer = 0
        i = pos
        while i > 0:
            answer += self.tree[i]
            i -= self.lowbit(i)
        return answer

    def range_sum(self, left: int, right: int) -> int:
        return self.prefix_sum(right) - self.prefix_sum(left - 1)


def main() -> None:
    data = sys.stdin.buffer.read().split()
    if not data:
        return
    it = iter(map(int, data))

    n = next(it)
    m = next(it)

    bit = Fenwick(n)
    for i in range(1, n + 1):
        bit.add(i, next(it))

    out: list[str] = []
    for _ in range(m):
        operation = next(it)
        if operation == 1:
            pos = next(it)
            value = next(it)
            bit.add(pos, value)
        else:
            left = next(it)
            right = next(it)
            out.append(str(bit.range_sum(left, right)))

    if out:
        sys.stdout.write("\n".join(out) + "\n")


if __name__ == "__main__":
    main()
