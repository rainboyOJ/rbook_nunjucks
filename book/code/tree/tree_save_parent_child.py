# 树的存储：父亲孩子表示法
# 每个点记两个方向：fa[u] 是父亲，ch[u] 是孩子列表，向上向下都 O(1)。
# 孩子列表一共只装 n-1 个孩子，总空间 O(n)。
# 适用：树形 DP、DFS/BFS 遍历、换根这类两个方向都要的问题。
# 如何使用：
# 1. init(n)         # 多组数据必须调用
# 2. add_child(u, v) # u 是 v 的父亲
# 3. 向下：for v in ch[u] ... ；向上：fa[u]

fa: list[int] = []           # fa[u]：u 的父亲；fa[root] = 0，根没有父亲
ch: list[list[int]] = []     # ch[u]：u 的孩子，按加入顺序


def init(n: int) -> None:
    """把 1..n 清空，fa 全为 0（fa[root] = 0 正好表示"根没有父亲"）。"""
    global fa, ch
    fa = [0] * (n + 1)
    ch = [[] for _ in range(n + 1)]


def add_child(u: int, v: int) -> None:
    """u 是 v 的父亲：孩子挂到 ch[u]，父亲写进 fa[v]，加边 O(1)。"""
    ch[u].append(v)
    fa[v] = u
