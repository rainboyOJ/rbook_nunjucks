# 树的存储：孩子表示法
# 每个点只记孩子列表 ch[u]，不记父亲；孩子一共 n-1 个，空间 O(n)。
# 只适合向下走：枚举孩子 O(孩子数)；想向上找父亲只能把所有列表扫一遍 O(n)。
# 适用：只自顶向下遍历或递推、从不回头的问题。
# 如何使用：
# 1. init(n)         # 多组数据必须调用
# 2. add_child(u, v) # u 是 v 的父亲
# 3. 向下：for v in ch[u] ...
# 要向上跳就改用父亲孩子表示法（tree_save_parent_child.py）。

ch: list[list[int]] = []     # ch[u]：u 的孩子，按加入顺序


def init(n: int) -> None:
    """把 1..n 清空。"""
    global ch
    ch = [[] for _ in range(n + 1)]


def add_child(u: int, v: int) -> None:
    """u 是 v 的父亲：孩子挂到 ch[u]，加边 O(1)。"""
    ch[u].append(v)
