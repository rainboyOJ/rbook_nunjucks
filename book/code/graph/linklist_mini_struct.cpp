struct linkList {
    struct edge {
        int u, v, w, next;
    };
    edge e[maxe];
    int h[maxn], edge_cnt;
    linkList() {
        edge_cnt = 0;
        memset(h, -1, sizeof(h));
    }
    void add(int u, int v, int w = 0) {
        e[edge_cnt] = {u, v, w, h[u]};
        h[u] = edge_cnt++;
    }
    void add2(int u, int v, int w = 0) {
        add(u, v, w);
        add(v, u, w);
    }
} e;

// 如何遍历u为起点的所有边
for (int i = e.h[u]; i != -1; i = e.e[i].next) {
    int v = e.e[i].v;
    int w = e.e[i].w;
    // do something
}
