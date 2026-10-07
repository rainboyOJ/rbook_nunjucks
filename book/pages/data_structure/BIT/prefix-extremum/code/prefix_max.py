# 树状数组维护前缀最大值：只支持单点「变大」+ 前缀 max。
# 下标从 1 开始，lowbit(0) = 0 会让循环原地打转。
# 不变量：chmax 只让值单调不减；一旦把某个位置改小，答案就错了。
# C++ 用 numeric_limits<long long>::lowest() 当单位元，这里取 -(2**63)。

import sys

IDENTITY = -(1 << 63)


class FenwickPrefixMax:
    """tree[i] 维护 a[i - lowbit(i) + 1 .. i] 的最大值。"""

    def __init__(self, size: int = 0) -> None:
        self.n = size
        self.tree = [IDENTITY] * (size + 1)

    @staticmethod
    def lowbit(x: int) -> int:
        # 最低位那个 1 代表的块长，即 x & -x。
        return x & -x

    def chmax(self, pos: int, value: int) -> None:
        i = pos
        while i <= self.n:
            if value > self.tree[i]:
                self.tree[i] = value
            i += self.lowbit(i)

    def prefix_max(self, pos: int) -> int:
        answer = IDENTITY
        i = pos
        while i > 0:
            if self.tree[i] > answer:
                answer = self.tree[i]
            i -= self.lowbit(i)
        return answer


def main() -> None:
    data = sys.stdin.buffer.read().split()
    if not data:
        return
    it = iter(map(int, data))

    n = next(it)
    m = next(it)

    bit = FenwickPrefixMax(n)
    for i in range(1, n + 1):
        bit.chmax(i, next(it))

    out: list[str] = []
    for _ in range(m):
        operation = next(it)
        if operation == 1:
            pos = next(it)
            value = next(it)
            bit.chmax(pos, value)
        else:
            right = next(it)
            out.append(str(bit.prefix_max(right)))

    if out:
        sys.stdout.write("\n".join(out) + "\n")


if __name__ == "__main__":
    main()
