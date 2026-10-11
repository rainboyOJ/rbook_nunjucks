# 完整实现：从无向边输入建一棵有根树，输出每个点的深度和子树大小
# 输入：
#   第一行 n root
#   接下来 n-1 行 u v（无向边）
# 输出：
#   第一行 depth[1..n]，第二行 size[1..n]
#
# 两种表示法配合使用：
#   孩子表示法 ch[]：自顶向下求深度
#   父亲表示法 fa[]：按 DFS 逆序自底向上累加子树大小

import sys

def main() -> None:
    it = iter(sys.stdin.read().split())
    n = int(next(it))
    root = int(next(it))

    # 第一步：树是特殊的图，先用邻接表存 n-1 条无向边
    g: list[list[int]] = [[] for _ in range(n + 1)]
    for _ in range(n - 1):
        u = int(next(it))
        v = int(next(it))
        g[u].append(v)
        g[v].append(u)

    ch: list[list[int]] = [[] for _ in range(n + 1)]  # 孩子表示法
    fa: list[int] = [0] * (n + 1)                     # 父亲表示法
    depth: list[int] = [0] * (n + 1)                  # 深度：到根的路径上的边数
    sz: list[int] = [1] * (n + 1)                     # 子树大小，先算上自己

    # 从 root 出发 DFS（用栈模拟），确定每个点的父亲和孩子，同时得到遍历顺序
    order: list[int] = []  # DFS 顺序：父亲一定排在孩子前面
    stk = [root]
    fa[root] = 0  # 根没有父亲
    while stk:
        u = stk.pop()
        order.append(u)
        for v in g[u]:
            if v == fa[u]:  # 树上 u 只有父亲这一条"回头边"
                continue
            fa[v] = u       # 第一次走到 v 的点就是 v 的父亲
            ch[u].append(v)
            depth[v] = depth[u] + 1
            stk.append(v)

    # 自底向上（父亲表示法）：逆序遍历，把子树大小累加到父亲上
    for u in reversed(order):
        if u != root:
            sz[fa[u]] += sz[u]

    print("depth:", *(depth[i] for i in range(1, n + 1)))
    print("size:", *(sz[i] for i in range(1, n + 1)))


if __name__ == "__main__":
    main()
