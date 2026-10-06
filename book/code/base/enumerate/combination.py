# 枚举所有 m 组合，DFS 按"只往右选"保证每个组合恰好出现一次。
# a 会按输入顺序被选择；如果希望字典序输出，调用前先对 a 排序。
# emit 收到的 path 是内部列表的引用：要保留就 copy，不要边遍历边改。
# 深度最多 m（组合大小），Python 默认递归上限 1000 内足够；m 逼近 1e5 才需 sys.setrecursionlimit。

from collections.abc import Callable
from typing import TypeVar

T = TypeVar("T")


def enumerate_combinations(a: list[T], m: int, emit: Callable[[list[T]], None]) -> None:
    n = len(a)
    if m < 0 or m > n:
        return

    path: list[T] = []

    # need 是还差多少个元素：i 超过 n - need 时凑不满，直接剪枝。
    def dfs(last: int) -> None:
        if len(path) == m:
            emit(path)
            return

        need = m - len(path)
        for i in range(last + 1, n - need + 1):
            path.append(a[i])
            dfs(i)
            path.pop()

    dfs(-1)
