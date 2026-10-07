# 树的直径（两遍最远点搜索）：从 1 找最远点 a，再从 a 找最远点 b，dis[b] 即直径。
# 带权边，权值为整数；n 可达 1e6，用 BFS 迭代而非递归。边权可较大，
# C++ 用 long long 防溢出，Python int 任意精度无此问题。
import sys
from collections import deque


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

    def farthest(s: int) -> tuple[int, list[int]]:
        dis = [-1] * (n + 1)
        dis[s] = 0
        q: deque[int] = deque([s])
        far = s
        while q:
            u = q.popleft()
            if dis[u] > dis[far]:
                far = u
            for v, w in adj[u]:
                if dis[v] != -1:
                    continue
                dis[v] = dis[u] + w
                q.append(v)
        return far, dis

    a, _ = farthest(1)
    b, dis = farthest(a)
    print(dis[b])


if __name__ == "__main__":
    main()
