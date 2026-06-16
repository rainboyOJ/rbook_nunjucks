#include <bits/stdc++.h>
using namespace std;

struct SegmentTree {
    vector<long long> sum;
    vector<long long> lazy;
    int mod = 1;

    SegmentTree(int n = 0, int mod_value = 1) {
        init(n, mod_value);
    }

    void init(int n, int mod_value) {
        mod = mod_value;
        sum.assign(n * 4 + 5, 0);
        lazy.assign(n * 4 + 5, 0);
    }

    void apply(int u, int l, int r, long long value) {
        value %= mod;
        sum[u] = (sum[u] + value * (r - l + 1)) % mod;
        lazy[u] = (lazy[u] + value) % mod;
    }

    void pushdown(int u, int l, int r) {
        if (lazy[u] == 0 || l == r) return;
        int mid = (l + r) >> 1;
        apply(u << 1, l, mid, lazy[u]);
        apply(u << 1 | 1, mid + 1, r, lazy[u]);
        lazy[u] = 0;
    }

    void build(int u, int l, int r, const vector<long long> &base) {
        if (l == r) {
            sum[u] = base[l] % mod;
            return;
        }
        int mid = (l + r) >> 1;
        build(u << 1, l, mid, base);
        build(u << 1 | 1, mid + 1, r, base);
        sum[u] = (sum[u << 1] + sum[u << 1 | 1]) % mod;
    }

    void range_add(int ql, int qr, long long value, int u, int l, int r) {
        if (ql <= l && r <= qr) {
            apply(u, l, r, value);
            return;
        }
        pushdown(u, l, r);
        int mid = (l + r) >> 1;
        if (ql <= mid) range_add(ql, qr, value, u << 1, l, mid);
        if (qr > mid) range_add(ql, qr, value, u << 1 | 1, mid + 1, r);
        sum[u] = (sum[u << 1] + sum[u << 1 | 1]) % mod;
    }

    long long range_sum(int ql, int qr, int u, int l, int r) {
        if (ql <= l && r <= qr) return sum[u];
        pushdown(u, l, r);
        int mid = (l + r) >> 1;
        long long answer = 0;
        if (ql <= mid) answer += range_sum(ql, qr, u << 1, l, mid);
        if (qr > mid) answer += range_sum(ql, qr, u << 1 | 1, mid + 1, r);
        return answer % mod;
    }
};

struct HeavyLightDecomposition {
    int n;
    int root;
    int mod;
    int timer = 0;
    vector<vector<int>> graph;
    vector<int> parent, depth, subtree_size, heavy_son;
    vector<int> top, dfn, node_at;
    vector<long long> value, ordered_value;
    SegmentTree seg;

    HeavyLightDecomposition(int n, int root, int mod)
        : n(n), root(root), mod(mod),
          graph(n + 1),
          parent(n + 1), depth(n + 1), subtree_size(n + 1),
          heavy_son(n + 1, 0), top(n + 1), dfn(n + 1), node_at(n + 1),
          value(n + 1), ordered_value(n + 1),
          seg(n, mod) {}

    void add_edge(int u, int v) {
        graph[u].push_back(v);
        graph[v].push_back(u);
    }

    void dfs_size(int u, int father) {
        parent[u] = father;
        depth[u] = depth[father] + 1;
        subtree_size[u] = 1;
        heavy_son[u] = 0;

        for (int v : graph[u]) {
            if (v == father) continue;
            dfs_size(v, u);
            subtree_size[u] += subtree_size[v];
            if (heavy_son[u] == 0 ||
                subtree_size[v] > subtree_size[heavy_son[u]]) {
                heavy_son[u] = v;
            }
        }
    }

    void dfs_decompose(int u, int chain_top) {
        top[u] = chain_top;
        dfn[u] = ++timer;
        node_at[timer] = u;
        ordered_value[timer] = value[u];

        if (heavy_son[u] != 0) {
            dfs_decompose(heavy_son[u], chain_top);
        }

        for (int v : graph[u]) {
            if (v == parent[u] || v == heavy_son[u]) continue;
            dfs_decompose(v, v);
        }
    }

    void build() {
        dfs_size(root, 0);
        dfs_decompose(root, root);
        seg.build(1, 1, n, ordered_value);
    }

    void path_add(int u, int v, long long delta) {
        while (top[u] != top[v]) {
            if (depth[top[u]] < depth[top[v]]) swap(u, v);
            seg.range_add(dfn[top[u]], dfn[u], delta, 1, 1, n);
            u = parent[top[u]];
        }
        if (depth[u] > depth[v]) swap(u, v);
        seg.range_add(dfn[u], dfn[v], delta, 1, 1, n);
    }

    long long path_sum(int u, int v) {
        long long answer = 0;
        while (top[u] != top[v]) {
            if (depth[top[u]] < depth[top[v]]) swap(u, v);
            answer += seg.range_sum(dfn[top[u]], dfn[u], 1, 1, n);
            answer %= mod;
            u = parent[top[u]];
        }
        if (depth[u] > depth[v]) swap(u, v);
        answer += seg.range_sum(dfn[u], dfn[v], 1, 1, n);
        return answer % mod;
    }

    void subtree_add(int u, long long delta) {
        seg.range_add(dfn[u], dfn[u] + subtree_size[u] - 1, delta, 1, 1, n);
    }

    long long subtree_sum(int u) {
        return seg.range_sum(dfn[u], dfn[u] + subtree_size[u] - 1, 1, 1, n);
    }
};

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n, m, root, mod;
    cin >> n >> m >> root >> mod;

    HeavyLightDecomposition hld(n, root, mod);
    for (int i = 1; i <= n; i++) {
        cin >> hld.value[i];
        hld.value[i] %= mod;
    }

    for (int i = 1; i < n; i++) {
        int u, v;
        cin >> u >> v;
        hld.add_edge(u, v);
    }

    hld.build();

    while (m--) {
        int op;
        cin >> op;
        if (op == 1) {
            int x, y;
            long long z;
            cin >> x >> y >> z;
            hld.path_add(x, y, z);
        } else if (op == 2) {
            int x, y;
            cin >> x >> y;
            cout << hld.path_sum(x, y) << '\n';
        } else if (op == 3) {
            int x;
            long long z;
            cin >> x >> z;
            hld.subtree_add(x, z);
        } else {
            int x;
            cin >> x;
            cout << hld.subtree_sum(x) << '\n';
        }
    }

    return 0;
}
