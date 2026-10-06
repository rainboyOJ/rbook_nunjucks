# 用模块级全局状态枚举所有子集，与 C++ 版结构一一对应。
# 调用前按下述顺序布置（a 为 1 下标，a[0] 不用）：
#   subset.n = n
#   subset.a = [0] + a
#   subset.path = [0] * (n + 1)    # C++ 里是静态定长数组，Python 需显式建好，否则越界
#   subset.res = []                # C++ 在递归内直接 cout，这里收集已选元素的切片副本
#   subset.dfs(1, 0)
# 每次进入 dfs 都记录当前已选元素（含空集）；深度最多 n + 1，
# n 在 30+5 量级（C++ maxn），远低于默认递归上限，无需 sys.setrecursionlimit。

n: int = 0
a: list[int] = []
path: list[int] = []
res: list[list[int]] = []


def dfs(dep: int, last: int) -> None:
    # 进入 dfs 时，path[1..dep-1] 是当前已选元素；append 副本防止后续递归复写。
    res.append(path[1:dep].copy())

    for i in range(last + 1, n + 1):
        path[dep] = a[i]
        dfs(dep + 1, i)
