# 最长严格上升子序列（LIS）长度，二分优化 O(n log n)。
# tail[k] 是「长度为 k+1 的严格上升子序列」末尾元素的最小可能值，tail 本身严格递增。
# a 用 0 下标；空序列（n = 0）返回 0。

from bisect import bisect_left


def lis_binary(a: list[int]) -> int:
    """返回 a 的最长严格上升子序列长度。"""
    tail: list[int] = []
    for x in a:
        # 对应 C++ 的 lower_bound：第一个 >= x 的位置。
        # 严格上升用 bisect_left（若求非严格上升，应改用 bisect_right）。
        pos = bisect_left(tail, x)
        if pos == len(tail):
            tail.append(x)  # x 比所有结尾都大，可以延长最长长度
        else:
            tail[pos] = x  # 换成更小的结尾，不影响已有长度的可行性
    return len(tail)
