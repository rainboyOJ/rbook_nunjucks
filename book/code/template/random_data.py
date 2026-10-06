# 通用随机数据工具：rnd(l, r) 取闭区间随机整数，MyShuffle 生成 [1, n] 的随机排列。
# 随机源是模块级 _rng（对应 C++ 全局 __rnd / mtrnd）；复现前先 _rng.seed(固定种子)。
# 本文件不含 main 与输入输出，调用示例：_rng.seed(1); n = rnd(4, 7)。

import random

_rng = random.Random()


def rnd(l: int, r: int) -> int:
    """返回 [l, r] 内的随机整数；randrange 无 C++ 取模偏置，边界含两端。"""
    return _rng.randrange(l, r + 1)


class MyShuffle:
    """构造时把 [1, n] 洗成随机排列，get() 依次吐出，共 n 个不重复值。"""

    def __init__(self, n: int) -> None:
        tail = list(range(1, n + 1))
        _rng.shuffle(tail)
        # a[0] 是占位，与 C++ 的 1 下标数组对齐；get() 从 a[1] 开始取。
        self.a = [0] + tail
        self.idx = 0

    def get(self) -> int:
        self.idx += 1
        return self.a[self.idx]
