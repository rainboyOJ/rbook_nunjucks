# 树的重心：输出重心个数与所有重心编号（升序，空格分隔）。
# 重心 = 删除该点后最大连通块大小最小的点。
# n 可达 1e6，递归 DFS 会爆 Python 栈，这里改用显式栈做迭代后序遍历。
import sys


def main() -> None:
    data = sys.stdin.buffer.read().split()
    if not data:
        return
    pos = 0
    n = int(data[pos]); pos += 1

    adj: list[list[int]] = [[] for _ in range(n + 1)]
    for _ in range(n - 1):
        u = int(data[pos]); pos += 1
        v = int(data[pos]); pos += 1
        adj[u].append(v)
        adj[v].append(u)

    # 迭代求先序序列，再逆序处理即可保证儿子先于父亲被访问。
    parent = [0] * (n + 1)
    order: list[int] = []
    visited = [False] * (n + 1)
    stack = [1]
    visited[1] = True
    while stack:
        u = stack.pop()
        order.append(u)
        for v in adj[u]:
            if not visited[v]:
                visited[v] = True
                parent[v] = u
                stack.append(v)

    sz = [1] * (n + 1)
    best = n
    ans: list[int] = []
    for u in reversed(order):
        sz[u] = 1
        mx = 0  # B(u)：先看各儿子子树
        for v in adj[u]:
            if v == parent[u]:
                continue
            sz[u] += sz[v]
            if sz[v] > mx:
                mx = sz[v]
        # 父亲方向也是一块：整棵树减去 u 的子树
        if n - sz[u] > mx:
            mx = n - sz[u]
        if mx < best:
            best = mx
            ans = [u]
        elif mx == best:
            ans.append(u)

    ans.sort()
    sys.stdout.write(str(len(ans)) + "\n")
    sys.stdout.write(" ".join(map(str, ans)) + "\n")


if __name__ == "__main__":
    main()
