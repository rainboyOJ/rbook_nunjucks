# 交换相邻两个半开区间 [first, middle) 与 [middle, last)：三次 reverse 实现。
# 对拍时 C++ 侧用同参数调用；函数不检查下标合法性，与 C++ 迭代器行为一致。
# 注：C++ 模板参数是迭代器，Python 版改用下标（list 无迭代器算术），
# 语义等价：作用于 a[l:r] 这段连续内存，函数外其余元素不受影响。

from typing import TypeVar

T = TypeVar("T")


def reverse_range(a: list[T], first: int, last: int) -> None:
    # 对应 std::reverse：原地反转半开区间 [first, last)。
    i = first
    j = last - 1
    while i < j:
        a[i], a[j] = a[j], a[i]
        i += 1
        j -= 1


def swap_adjacent_ranges(a: list[T], first: int, middle: int, last: int) -> None:
    # 反转前段、反转后段、整体反转，恰好完成两段的交换且各自内部顺序不变。
    reverse_range(a, first, middle)
    reverse_range(a, middle, last)
    reverse_range(a, first, last)
