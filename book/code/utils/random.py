# 随机数工具：模块级 rng 在导入时用系统熵播种（对应 C++ 用 steady_clock 播种），
# 无需布置任何全局量，直接调用即可。调用示例：x = rnd(1, 6)。
# 要求 l <= r；l > r 在 C++ 的 uniform_int_distribution 里是 UB，这里 randrange 抛 ValueError。
# 易错点：random.Random 没有 randbelow，randrange(k) 才是 [0, k) 上的均匀整数。

import random

# 全局发生器，对应 C++ 的 mt19937 rng。
rng: random.Random = random.Random()


def rnd(l: int, r: int) -> int:
    """生成 [l, r] 闭区间上的均匀整数，与 uniform_int_distribution<int>(l, r) 一致。"""
    return l + rng.randrange(r - l + 1)
