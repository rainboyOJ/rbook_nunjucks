const int maxn = 1e6 + 5;
vector<pair<int, int>> g[maxn]; // 带权：g[u] 里存 (v, w)；无权就把 pair<int, int> 换成 int

void add_edge(int u, int v, int w) {
    g[u].push_back({v, w});
    // 加无向边,再来一次
    // g[v].push_back({u, w});
}

// 遍历 u 的出边：顺序 = 加边顺序
// for (auto [v, w] : g[u]) { ... }
