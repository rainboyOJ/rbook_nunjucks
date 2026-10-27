# 依赖模块级全局邻接表 tree：使用前先 tree = [[] for _ in range(n + 1)]，
# 再加无权边 tree[u].append(v) 和 tree[v].append(u)。点编号 1..n，根的 in_ 是 1。
# in_ / out 是进入、退出时间戳，子树 u 恰好占区间 [in_[u], out[u]]，长度就是子树大小。
# 深树（链）递归会爆栈，所以这里用显式栈迭代；in 是 Python 关键字，故写作 in_。

type Graph = list[list[int]]

tree: Graph = []


class DFSOrder:
    """DFS 序（先序时间戳）与欧拉序。"""

    n: int
    in_: list[int]
    out: list[int]
    sz: list[int]
    dep: list[int]
    order: list[int]  # order[i] = 第 i 个被访问的点
    euler: list[int]  # 欧拉序，长度 2n-1（不是先序！每个点会重复出现）
    timer: int
    etot: int

    def __init__(self, n: int) -> None:
        self.n = n
        self.in_ = [0] * (n + 1)
        self.out = [0] * (n + 1)
        self.sz = [0] * (n + 1)
        self.dep = [0] * (n + 1)
        self.order = [0] * (n + 1)
        self.euler = [0] * (2 * n)
        self.timer = 0
        self.etot = 0

    def build(self, root: int = 1) -> None:
        """迭代 DFS 求先序时间戳；多组数据必须重建对象（或把 timer 清 0）。"""
        self.timer = 0
        # 栈元素 (点, 父亲, 下一条待处理的邻边下标)，下标 0 兼作“还没进入”的标记。
        stack: list[tuple[int, int, int]] = [(root, 0, 0)]
        while stack:
            u, fa, idx = stack.pop()
            if idx == 0:  # 第一次弹到 u：等价于递归版的“进入”
                self.timer += 1
                self.in_[u] = self.timer
                self.order[self.timer] = u
                self.sz[u] = 1

            advanced = False
            while idx < len(tree[u]):
                v = tree[u][idx]
                idx += 1
                if v == fa:
                    continue
                self.dep[v] = self.dep[u] + 1
                stack.append((u, fa, idx))  # 记住自己处理到哪条边，回来继续
                stack.append((v, u, 0))
                advanced = True
                break

            if not advanced:  # 邻边走完：等价于递归版的“退出”
                self.out[u] = self.timer
                if stack:
                    self.sz[stack[-1][0]] += self.sz[u]  # 子树大小交回父亲

    def is_ancestor(self, u: int, v: int) -> bool:
        """O(1) 判断 u 是不是 v 的祖先（u == v 也算）。"""
        return self.in_[u] <= self.in_[v] <= self.out[u]

    def subtree_size(self, u: int) -> int:
        return self.out[u] - self.in_[u] + 1  # 恒等于 self.sz[u]

    def build_euler(self, root: int = 1) -> None:
        """欧拉序：进入写一次，每从一个儿子返回再写一次，长度 2n-1。"""

        def dfs(u: int, fa: int) -> None:
            self.etot += 1
            self.euler[self.etot] = u
            for v in tree[u]:
                if v == fa:
                    continue
                dfs(v, u)
                self.etot += 1
                self.euler[self.etot] = u

        self.etot = 0
        dfs(root, 0)
