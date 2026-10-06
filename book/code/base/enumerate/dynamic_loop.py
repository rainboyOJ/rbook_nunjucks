# 递归实现 n 层循环，每层都从 [0, m) 中选择一个值，共 m^n 个序列。
# emit(path) 会收到一个长度为 n 的完整序列；path 是内部列表的引用，要保留就 copy。
# 注意：m^n 个序列会全部流经 emit，n 稍大就爆炸，调用方自己控制规模。
# 深度恰好 n + 1：n 逼近 1e5 时需要 sys.setrecursionlimit 或改写迭代。

from collections.abc import Callable


def enumerate_dynamic_loop(n: int, m: int, emit: Callable[[list[int]], None]) -> None:
    if n < 0 or m < 0:
        return

    path = [0] * n

    def dfs(dep: int) -> None:
        if dep == n:
            emit(path)
            return

        for x in range(m):
            path[dep] = x
            dfs(dep + 1)

    dfs(0)
