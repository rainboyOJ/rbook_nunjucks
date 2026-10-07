# 洛谷 P3372 线段树 1（树状数组写法）：区间加 + 区间和。
# 两棵树同步维护：bit_diff 存差分 d，bit_weighted 存 i * d_i。
#   prefix_sum(x) = (x + 1) * sum(d[1..x]) - sum(i * d_i, 1..x)
# 下标从 1 开始，lowbit(0) = 0 会让循环原地打转。
# C++ 里 index * value 可能超过 int，用 long long；Python int 任意精度，不必特判。

import sys


class RangeFenwick:
    """tree[i] 维护对应数组在区间 (i - lowbit(i), i] 上的和。"""

    def __init__(self, size: int = 0) -> None:
        self.n = size
        self.bit_diff = [0] * (size + 1)
        self.bit_weighted = [0] * (size + 1)

    @staticmethod
    def lowbit(x: int) -> int:
        # 最低位那个 1 代表的块长，即 x & -x。
        return x & -x

    def add(self, bit: list[int], pos: int, value: int) -> None:
        i = pos
        while i <= self.n:
            bit[i] += value
            i += self.lowbit(i)

    def sum(self, bit: list[int], pos: int) -> int:
        answer = 0
        i = pos
        while i > 0:
            answer += bit[i]
            i -= self.lowbit(i)
        return answer

    def range_add(self, left: int, right: int, value: int) -> None:
        # 差分：左端点 +value，右端点后一位 -value。
        self.add(self.bit_diff, left, value)
        self.add(self.bit_diff, right + 1, -value)
        # 加权树同步：位置 i 的增量是 i * d_i，右端点后一位用 r + 1。
        self.add(self.bit_weighted, left, value * left)
        self.add(self.bit_weighted, right + 1, -value * (right + 1))

    def prefix_sum(self, pos: int) -> int:
        return (pos + 1) * self.sum(self.bit_diff, pos) - self.sum(self.bit_weighted, pos)

    def range_sum(self, left: int, right: int) -> int:
        return self.prefix_sum(right) - self.prefix_sum(left - 1)


def main() -> None:
    data = sys.stdin.buffer.read().split()
    if not data:
        return
    it = iter(map(int, data))

    n = next(it)
    m = next(it)

    bit = RangeFenwick(n)
    for i in range(1, n + 1):
        # 用区间加 (i, i) 建树，与 C++ 逐点写入的做法一致。
        bit.range_add(i, i, next(it))

    out: list[str] = []
    for _ in range(m):
        operation = next(it)
        left = next(it)
        right = next(it)
        if operation == 1:
            value = next(it)
            bit.range_add(left, right, value)
        else:
            out.append(str(bit.range_sum(left, right)))

    if out:
        sys.stdout.write("\n".join(out) + "\n")


if __name__ == "__main__":
    main()
