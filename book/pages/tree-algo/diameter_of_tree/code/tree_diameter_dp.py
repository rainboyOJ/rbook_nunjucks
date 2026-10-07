# 树的直径（树形 DP）：f[u] 为从 u 往下走的最长链长度。
# 处理 u 的儿子 v 时，先 ans = max(ans, f[u] + f[v] + w)（此时 f[u] 不含 v），
# 再 f[u] = max(f[u], f[v] + w)。n 可达 1e6，递归 DFS 会爆 Python 栈，改迭代后序遍历。
# C++ 用 long long 存答案防溢出，Python int 任意精度无此问题。
import sys


def main() -> None:
    data = sys.stdin.buffer.read().split()
    if not data:
        return
    pos = 0
    n = int(data[pos]); pos += 1

    adj: list[list[tuple[int, int]]] = [[] for _ in range(n + 1)]
    for _ in range(n - 1):
        u = int(data[pos]); pos += 1
        v = int(data[pos]); pos += 1
        w = int(data[pos]); pos += 1
        adj[u].append((v, w))
        adj[v].append((u, w))

    # 先序序列逆序即为「儿子先于父亲」的处理顺序。
    parent = [0] * (n + 1)
    order: list[int] = []
    visited = [False] * (n + 1)
    stack = [1]
    visited[1] = True
    while stack:
        u = stack.pop()
        order.append(u)
        for v, _w in adj[u]:
            if not visited[v]:
                visited[v] = True
                parent[v] = u
                stack.append(v)

    f = [0] * (n + 1)
    ans = 0
    for u in reversed(order):
        for v, w in adj[u]:
            if v == parent[u]:
                continue
            if f[u] + f[v] + w > ans:
                ans = f[u] + f[v] + w
            if f[v] + w > f[u]:
                f[u] = f[v] + w

    print(ans)


if __name__ == "__main__":
    main()
