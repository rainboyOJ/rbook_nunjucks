# 洛谷 P3368 树状数组 2：区间加 + 单点查。
# 树状数组维护差分数组 d，point_query(x) = sum(d[1..x]) 才是原数组 a[x]。
# 下标从 1 开始，lowbit(0) = 0 会让循环原地打转。
# C++ 用 long long 防溢出；Python int 是任意精度，不必特判。

import sys


class RangeAddPointQueryFenwick:
    """tree[i] 维护差分数组 d[i - lowbit(i) + 1 .. i] 的元素和。"""

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

    def range_add(self, left: int, right: int, value: int) -> None:
        self.add(left, value)
        # 只有 right < n 时 right + 1 才是合法差分下标；等于 n + 1 时 add 循环不进入，可省略。
        if right < self.n:
            self.add(right + 1, -value)

    def point_query(self, pos: int) -> int:
        return self.prefix_sum(pos)


def main() -> None:
    data = sys.stdin.buffer.read().split()
    if not data:
        return
    it = iter(map(int, data))

    n = next(it)
    m = next(it)

    bit = RangeAddPointQueryFenwick(n)
    previous = 0
    for i in range(1, n + 1):
        value = next(it)
        # 初始化喂的是差分 a[i] - a[i-1]，不是 a[i] 本身。
        bit.add(i, value - previous)
        previous = value

    out: list[str] = []
    for _ in range(m):
        operation = next(it)
        if operation == 1:
            left = next(it)
            right = next(it)
            value = next(it)
            bit.range_add(left, right, value)
        else:
            pos = next(it)
            out.append(str(bit.point_query(pos)))

    if out:
        sys.stdout.write("\n".join(out) + "\n")


if __name__ == "__main__":
    main()
