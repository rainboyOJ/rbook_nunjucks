/**
 * linklist 的简单实现
 */
const int maxn = 1e6 + 5;
// 边结构
struct Edge {
    int to, next, w;
} e[maxn << 1];

int head[maxn], cnt;

void add_edge(int u, int v, int w = 1) {
    e[++cnt] = {v, head[u], w};
    head[u] = cnt;
}
void add_edge2(int u, int v, int w = 1) {
    add_edge(u, v, w);
    add_edge(v, u, w);
}

// i = 0 表示无边
for (int i = head[u]; i; i = e[i].next) {
    int v = e[i].to;
    // ...
}
