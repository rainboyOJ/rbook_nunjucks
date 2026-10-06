# 枚举输入序列的所有排列：used 标记每个下标是否已被选，dep 表示当前填写到第几位。
# 与 C++ 版相同，用模块级全局状态，调用前按下述顺序布置（a 为 1 下标，a[0] 不用）：
#   permutation.n = n
#   permutation.a = [0] + a
#   permutation.path = [0] * (n + 1)    # C++ 里是静态定长数组，Python 需显式建好，否则越界
#   permutation.used = [False] * (n + 1)
#   permutation.res = []                # C++ 在递归边界直接 cout，这里收集结果
#   permutation.dfs(1)
# 深度为 n + 1，n 在千以内无需调整递归上限。

n: int = 0
a: list[int] = []
path: list[int] = []
used: list[bool] = []
res: list[list[int]] = []


def dfs(dep: int) -> None:
    if dep > n:
        # 必须存副本：path 会被后续递归继续复写；只取填写的 1..n 位。
        res.append(path[1:n + 1].copy())
        return

    for i in range(1, n + 1):
        if used[i]:
            continue
        used[i] = True
        path[dep] = a[i]
        dfs(dep + 1)
        used[i] = False
