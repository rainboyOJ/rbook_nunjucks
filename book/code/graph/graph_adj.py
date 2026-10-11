# 邻接表存图（Python）：日常首选。g[u] 是 u 的出边列表，加边就是 append，遍历就是 for，没有指针和链表。
# 顶点 0..n-1 用形态一（列表套列表）；顶点编号稀疏/不连续用形态二（defaultdict），遍历写法完全相同。
# 什么时候改用链式前向星（linklist.py）：需要给边一个稳定编号时，例如网络流的 i ^ 1 反向边、给边打标记。
# 无向边 = 两条有向边，各 append 一次；遍历顺序 = 加边顺序（链式前向星是头插法，顺序相反）。

from collections import defaultdict

# ===== 形态一：顶点 0..n-1（最常用）=====

n = 4                        # 顶点数，编号 0..n-1；按题面赋值
g = [[] for _ in range(n)]   # 列表套列表：每个 g[u] 是 u 的出边列表

# 加有向边 u -> v：带权存 (v, w)，无权直接存 v
g[0].append((1, 5))
g[0].append((2, 7))
g[1].append((3, 9))

# 加无向边 u - v：两个方向各存一次
g[0].append((1, 5))
g[1].append((0, 5))

# 遍历 u 的出边：顺序 = 加边顺序
for v, w in g[0]:
    pass

# 查询 u -> v 是否存在：扫 g[u]，O(出度)；要 O(1) 查边就换邻接矩阵
has_edge = any(v == 2 for v, _ in g[0])

# ===== 形态二：顶点编号稀疏/不连续 =====

h = defaultdict(list)   # 读不存在的点自动得到 []，不会 KeyError；空点不占额外判断
h[10].append((99, 1))   # 顶点编号可以是任意大整数，甚至字符串
for v, w in h[10]:
    pass

# 自检：加边顺序决定遍历顺序
assert [v for v, _ in g[0]] == [1, 2, 1]  # (1,5)、(2,7)、无向边 (1,5)
assert g[3] == []                          # 孤立点就是一个空列表
assert any(v == 2 for v, _ in g[0]) is has_edge
