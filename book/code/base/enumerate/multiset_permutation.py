# 多重集合排列：先排序压缩成 value + cnt，再 DFS 按桶取数，保证不产生重复排列。
# count 返回排列总数，enumerate 把每个排列交给 emit；path 是内部列表引用，要保留就 copy。
# C++ 版用 long long 组合数表防止计数溢出；Python int 任意精度，直接乘即可。

from collections.abc import Callable
from typing import TypeVar

T = TypeVar("T")


def count_distinct_permutations(a: list[T]) -> int:
    a = sorted(a)

    freq: list[int] = []
    i = 0
    while i < len(a):
        j = i
        while j < len(a) and a[j] == a[i]:
            j += 1
        freq.append(j - i)
        i = j

    n = len(a)
    # C[k][c] = 组合数 C(k, c)：从剩余 k 个位置里给某个值分 c 个。
    C = [[0] * (n + 1) for _ in range(n + 1)]
    for k in range(n + 1):
        C[k][0] = C[k][k] = 1
        for c in range(1, k):
            C[k][c] = C[k - 1][c - 1] + C[k - 1][c]

    ans = 1
    remaining = n
    for c in freq:
        ans *= C[remaining][c]
        remaining -= c
    return ans


def enumerate_multiset_permutations(a: list[T], emit: Callable[[list[T]], None]) -> None:
    a = sorted(a)

    value: list[T] = []
    cnt: list[int] = []
    for x in a:
        if not value or value[-1] != x:
            value.append(x)
            cnt.append(1)
        else:
            cnt[-1] += 1

    path: list[T] = [a[0]] * len(a) if a else []

    # 与 C++ 一致：空输入时直接 emit 一个空排列。
    if not a:
        emit(path)
        return

    def dfs(pos: int) -> None:
        if pos == len(a):
            emit(path)
            return

        for i in range(len(value)):
            if cnt[i] == 0:
                continue
            cnt[i] -= 1
            path[pos] = value[i]
            dfs(pos + 1)
            cnt[i] += 1

    dfs(0)
