# 树的 DFS 序：in_[u] / out[u] 是进入、离开 u 的时间戳，子树 u 恰好占时间戳区间 [in_[u], out[u]]。
# 1 下标：节点编号 1..n，dfs(u, fa) 里 fa = 0 表示根的父亲，depth[0] = 0。
# 判祖先：u 是 v 的祖先 <=> in_[u] <= in_[v] 且 out[v] <= out[u]。
# 树深最坏 n（1e5+），dfs 用显式栈迭代；C++ 字段名 in 在 Python 是关键字，故写作 in_。


class DFSOrder:
    """欧拉序（时间戳）：order[timer] 是第 timer 个被访问的节点。"""

    n: int
    timer: int
    tree: list[list[int]]
    in_: list[int]
    out: list[int]
    parent: list[int]
    depth: list[int]
    order: list[int]

    def __init__(self, n: int) -> None:
        self.n = n
        self.timer = 0
        # 邻接表按节点编号 1..n 开，tree[u] 直接放 u 的邻居。
        self.tree = [[] for _ in range(n + 1)]
        self.in_ = [0] * (n + 1)
        self.out = [0] * (n + 1)
        self.parent = [0] * (n + 1)
        self.depth = [0] * (n + 1)
        self.order = [0] * (n + 1)

    def add_edge(self, u: int, v: int) -> None:
        self.tree[u].append(v)
        self.tree[v].append(u)

    def dfs(self, u: int, fa: int) -> None:
        """从 u 出发遍历，fa 是父亲（0 表示无）。迭代模拟递归，避免深树爆栈。"""
        # 栈元素 (节点, 父亲, 下一条待处理的邻边下标)：下标 0 兼作“尚未进入”的标记。
        stack: list[tuple[int, int, int]] = [(u, fa, 0)]
        while stack:
            node, father, edge_index = stack.pop()
            if edge_index == 0:
                # 第一次进入 node：记父亲、深度、进入时间戳，等价于递归版的前几行。
                self.parent[node] = father
                self.depth[node] = self.depth[father] + 1
                self.timer += 1
                self.in_[node] = self.timer
                self.order[self.timer] = node

            neighbors = self.tree[node]
            advanced = False
            for i in range(edge_index, len(neighbors)):
                v = neighbors[i]
                if v == father:
                    continue
                # 先把“自己从 i + 1 继续”压回去，再压儿子，保证儿子先被处理完。
                stack.append((node, father, i + 1))
                stack.append((v, node, 0))
                advanced = True
                break

            if not advanced:
                # 邻边处理完说明子树已全部访问，当前 timer 就是子树的最后一个时间戳。
                self.out[node] = self.timer

    def is_ancestor(self, u: int, v: int) -> bool:
        return self.in_[u] <= self.in_[v] and self.out[v] <= self.out[u]
