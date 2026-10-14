from collections import deque

type Graph = list[list[int]]

def is_bipartite(g: Graph) -> bool:
    # 通常不用0号点
    n = len(g)-1 
    color = [0] * (n+5)

    # 图可能不连通
    for start in range(1,n+1):
        if color[start] == 0:
            color[start] = 1
            q = deque([start])
            while q:
                u = q.popleft()
                for v in g[u]:
                    if color[v] == 0:
                        color[v] = 3-color[u]
                        q.append(v)
                    elif color[u] == color[v]:
                        return False
    return True
