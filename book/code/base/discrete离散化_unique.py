# 排序去重式离散化：xs 有序且无重复，get 返回 1 下标编号（0 留作"不存在"哨兵）。
# 必须先 build 再查询；add 进来的值在 build 前不作任何假设。
# C++ 里 resize 截断只是扔掉尾部冗余；Python 直接重建列表，语义相同。

import bisect


class Discrete:
    xs: list[int]

    def __init__(self) -> None:
        self.xs = []

    def clear(self) -> None:
        self.xs = []

    def add(self, x: int) -> None:
        self.xs.append(x)

    def build(self) -> None:
        # sorted + 逐个去重等价于 C++ 的 sort + unique + erase。
        result: list[int] = []
        for x in sorted(self.xs):
            if not result or result[-1] != x:
                result.append(x)
        self.xs = result

    # 返回 x 离散化后的 1 下标编号。
    def get(self, x: int) -> int:
        return bisect.bisect_left(self.xs, x) + 1

    # 找不到时返回 -1，适合查询不确定是否出现过的值。
    def get_maybe(self, x: int) -> int:
        i = bisect.bisect_left(self.xs, x)
        if i == len(self.xs) or self.xs[i] != x:
            return -1
        return i + 1

    # 根据 1 下标编号找回原值。
    def origin(self, k: int) -> int:
        return self.xs[k - 1]

    def size(self) -> int:
        return len(self.xs)
