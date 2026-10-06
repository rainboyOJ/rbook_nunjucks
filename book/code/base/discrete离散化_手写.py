# 手写离散化：不依赖 unique，用双指针原地压缩排序后的数组。
# build 之后 xs[0..p] 有序且无重复，get 返回 1 下标编号。
# 注意：C++ 版没有 get_maybe（查不到时 lower_bound 也返回一个编号），Python 保持一致，
# 查询前先确认 x 确实出现过，或换用离散化_unique 版。

import bisect


class DiscreteManual:
    xs: list[int]

    def __init__(self) -> None:
        self.xs = []

    def add(self, x: int) -> None:
        self.xs.append(x)

    # 手写去重逻辑
    def build(self) -> None:
        if not self.xs:
            return

        self.xs.sort()

        p = 0
        for i in range(1, len(self.xs)):
            if self.xs[i] != self.xs[p]:
                p += 1
                self.xs[p] = self.xs[i]
        # 此时 xs[0..p] 是去重后的数组
        # C++ 的 resize 截断在 Python 里用切片删除尾部重复元素来对应。
        del self.xs[p + 1:]

    def get(self, x: int) -> int:
        return bisect.bisect_left(self.xs, x) + 1

    def origin(self, k: int) -> int:
        return self.xs[k - 1]
