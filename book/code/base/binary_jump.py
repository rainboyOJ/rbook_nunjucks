# 倍增二分：在 [start, n] 中找最后一个满足 check(pos) 的位置，与普通二分等价但按 2 的幂跳跃。
# 前提：check 单调，true ... true false ... false；n 必须可越界试探（代码里先判 nxt <= n）。

from collections.abc import Callable


def binary_jump_last_true(start: int, n: int, check: Callable[[int], bool]) -> int:
    """若 [start, n] 内没有任何满足 check 的位置，则原样返回 start。"""
    pos = start
    max_step = 1
    # 找到不超过 n 的最大 2 的幂作为初始步长，之后步长逐次减半。
    while (max_step << 1) <= n:
        max_step <<= 1

    step = max_step
    while step > 0:
        nxt = pos + step
        if nxt <= n and check(nxt):
            pos = nxt
        step >>= 1
    return pos
