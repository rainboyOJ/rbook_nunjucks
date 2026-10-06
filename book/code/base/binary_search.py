# 左闭右闭二分：在 [l, r] 中查找第一个满足 check(pos) 的位置。
# 要求 check 单调：false false ... false true true ... true；
# 调用时要保证 r 是一个真实或虚拟的可行位置（例如哨兵），否则可能死循环。

from collections.abc import Callable


def first_true(l: int, r: int, check: Callable[[int], bool]) -> int:
    """区间为空（l > r）时返回 l，与 C++ 行为一致。"""
    while l < r:
        # l + (r - l) // 2 防溢出的写法在 Python 没有必要（int 任意精度），
        # 保留写法是为了和 C++ 完全同构。
        mid = l + (r - l) // 2
        if check(mid):
            r = mid
        else:
            l = mid + 1
    return l
