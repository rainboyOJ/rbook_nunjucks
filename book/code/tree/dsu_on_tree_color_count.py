# DSU on tree（树上启发式合并）统计每个子树里「出现次数最多的颜色」编号之和。
# 调用顺序：solver = DsuOnTree(n, max_color)；填 solver.color[1..n]；add_edge 建树；
# solver.dfs_size(1, 0)；solver.dfs_solve(1, 0, True)；答案在 solver.answer[1..n]。
# 颜色编号 1..max_color，color_count 按 max_color + 1 开，节点编号 1..n。
# dfs_size / dfs_solve / add_subtree 递归深度最坏 n（1e5+），使用前需 sys.setrecursionlimit(1 << 20)。


type Seq = list[int]


class DsuOnTree:
    """重儿子保留、轻儿子重算：每个点最多被加 O(log n) 次。"""

    n: int
    graph: list[list[int]]
    color: Seq
    subtree_size: Seq
    heavy_child: Seq
    answer: Seq
    color_count: Seq
    max_count: int
    sum_color: int
    big_child: int

    def __init__(self, n: int, max_color: int) -> None:
        self.n = n
        self.graph = [[] for _ in range(n + 1)]
        self.color = [0] * (n + 1)
        self.subtree_size = [0] * (n + 1)
        self.heavy_child = [0] * (n + 1)
        self.answer = [0] * (n + 1)
        # 下标 0 是哨兵：subtree_size[heavy_child[u]] 在无重儿子时会读到它，必须为 0。
        self.color_count = [0] * (max_color + 1)
        self.max_count = 0
        self.sum_color = 0
        self.big_child = 0

    def add_edge(self, u: int, v: int) -> None:
        self.graph[u].append(v)
        self.graph[v].append(u)

    def dfs_size(self, u: int, parent: int) -> None:
        self.subtree_size[u] = 1
        for v in self.graph[u]:
            if v == parent:
                continue
            self.dfs_size(v, u)
            self.subtree_size[u] += self.subtree_size[v]
            if self.subtree_size[v] > self.subtree_size[self.heavy_child[u]]:
                self.heavy_child[u] = v

    def add_color(self, c: int, delta: int) -> None:
        self.color_count[c] += delta
        if delta > 0:
            # 只在加点时维护最大值；删点阶段靠 reset_state 整体清零。
            if self.color_count[c] > self.max_count:
                self.max_count = self.color_count[c]
                self.sum_color = c
            elif self.color_count[c] == self.max_count:
                self.sum_color += c

    def add_subtree(self, u: int, parent: int, delta: int) -> None:
        """把 u 子树里除 big_child 外的所有点按 delta 计入（重儿子已算过，跳过）。"""
        self.add_color(self.color[u], delta)
        for v in self.graph[u]:
            if v == parent or v == self.big_child:
                continue
            self.add_subtree(v, u, delta)

    def reset_state(self) -> None:
        # color_count 逐位清零等价 fill(begin, end, 0)。
        for i in range(len(self.color_count)):
            self.color_count[i] = 0
        self.max_count = 0
        self.sum_color = 0

    def dfs_solve(self, u: int, parent: int, keep: bool) -> None:
        for v in self.graph[u]:
            if v == parent or v == self.heavy_child[u]:
                continue
            self.dfs_solve(v, u, False)

        if self.heavy_child[u]:
            self.dfs_solve(self.heavy_child[u], u, True)
            self.big_child = self.heavy_child[u]

        self.add_subtree(u, parent, 1)
        self.big_child = 0
        self.answer[u] = self.sum_color

        if not keep:
            self.add_subtree(u, parent, -1)
            self.reset_state()
