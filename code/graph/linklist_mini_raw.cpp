/**
 * linklist 的简单实现
 */
const int maxn = 1e6+5;
// 边结构
struct Edge {
    int to, next, w;
} e[maxn << 1];

int head[maxn], cnt;

void add_edge(int u, int v, int w = 1) {
    e[++cnt] = {v, head[u], w};
    head[u] = cnt;
    e[++cnt] = {u, head[v], w};
    head[v] = cnt;
}