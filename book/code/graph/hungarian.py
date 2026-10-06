# 二分图最大匹配：左部点 1..n，右部点 1..m，返回最多能配多少对。
# match[v]=0 表示右部点 v 未匹配，vis[v] 是单次增广的访问标记（每轮清空）。
# dfs 改显式栈：增广路最坏 O(n) 长，递归写法在 n=1e5 时会爆 Python 栈。

type Graph = list[list[int]]  # g[u]：左部点 u 的右部邻居表，1 下标


class Hungarian:
    """右部点 v 的配对左部点存在 match[v]，0 号位当哨兵。"""

    n: int
    m: int
    g: Graph
    match: list[int]
    vis: list[int]

    def __init__(self, n: int, m: int) -> None:
        self.n = n
        self.m = m
        self.g = [[] for _ in range(n + 1)]
        self.match = [0] * (m + 1)
        self.vis = [0] * (m + 1)

    def add_edge(self, u: int, v: int) -> None:
        # 越界的边按 C++ 原样静默丢弃。
        if u < 1 or u > self.n or v < 1 or v > self.m:
            return
        self.g[u].append(v)

    def dfs(self, start: int) -> bool:
        """给左部点 start 找增广路：显式栈模拟递归，返回值与递归版一致。"""
        # 每个栈帧记：左部点、下一条待试邻边下标、本帧选中的右部点。
        stack_u = [start]
        stack_i = [0]
        stack_v = [-1]
        while stack_u:
            u = stack_u[-1]
            i = stack_i[-1]
            if i == len(self.g[u]):
                # 该点所有邻边试完，回溯失败。
                stack_u.pop()
                stack_i.pop()
                stack_v.pop()
                continue
            v = self.g[u][i]
            stack_i[-1] = i + 1  # 先推进下标，递归时也相当于 for 循环往后走
            if self.vis[v]:
                continue
            self.vis[v] = 1
            stack_v[-1] = v
            if self.match[v] == 0:
                # 找到空右部点：从最深帧回溯，逐帧改写 match[本帧选中点]=本帧左部点。
                for d in range(len(stack_u) - 1, -1, -1):
                    self.match[stack_v[d]] = stack_u[d]
                return True
            # v 已匹配，尝试把它的左部点挪到别处（对应递归 dfs(match[v])）。
            stack_u.append(self.match[v])
            stack_i.append(0)
            stack_v.append(-1)
        return False

    def max_matching(self) -> int:
        ans = 0
        self.match = [0] * (self.m + 1)  # 对应 C++ 的 fill(match, 0)
        for u in range(1, self.n + 1):
            self.vis = [0] * (self.m + 1)  # 对应 C++ 的 fill(vis, 0)
            if self.dfs(u):
                ans += 1
        return ans
