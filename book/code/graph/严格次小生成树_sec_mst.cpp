#include <bits/stdc++.h>
using namespace std;

const long long INF = (1LL << 62);
const long long NEG = -(1LL << 60);

struct Edge {
    int u, v;
    long long w;
    int id;
    bool used = false;

    bool operator<(const Edge& other) const {
        return w < other.w;
    }
};

struct DSU {
    vector<int> fa;

    explicit DSU(int n = 0) { init(n); }

    void init(int n) {
        fa.resize(n + 1);
        iota(fa.begin(), fa.end(), 0);
    }

    int find(int x) {
        return x == fa[x] ? x : fa[x] = find(fa[x]);
    }

    bool merge(int a, int b) {
        a = find(a);
        b = find(b);
        if (a == b) return false;
        fa[a] = b;
        return true;
    }
};

struct StrictSecondMST {
    int n, lg;
    vector<Edge> edges;
    vector<vector<pair<int, long long>>> tree;
    vector<int> depth;
    vector<vector<int>> up;
    vector<vector<long long>> mx1, mx2;

    explicit StrictSecondMST(int n) : n(n) {
        lg = 1;
        while ((1 << lg) <= n) lg++;
        tree.assign(n + 1, {});
        depth.assign(n + 1, 0);
        up.assign(lg, vector<int>(n + 1, 0));
        mx1.assign(lg, vector<long long>(n + 1, NEG));
        mx2.assign(lg, vector<long long>(n + 1, NEG));
    }

    void add_edge(int u, int v, long long w, int id) {
        edges.push_back({u, v, w, id, false});
    }

    void add_value(long long x, long long& a, long long& b) {
        if (x == NEG) return;
        if (x > a) {
            if (x != a) b = a;
            a = x;
        } else if (x < a && x > b) {
            b = x;
        }
    }

    pair<long long, long long> merge_pair(pair<long long, long long> x,
                                          pair<long long, long long> y) {
        long long a = NEG, b = NEG;
        add_value(x.first, a, b);
        add_value(x.second, a, b);
        add_value(y.first, a, b);
        add_value(y.second, a, b);
        return {a, b};
    }

    void dfs(int u, int fa) {
        for (auto [v, w] : tree[u]) {
            if (v == fa) continue;
            depth[v] = depth[u] + 1;
            up[0][v] = u;
            mx1[0][v] = w;
            dfs(v, u);
        }
    }

    void build_lca() {
        depth[1] = 1;
        dfs(1, 0);

        for (int k = 1; k < lg; k++) {
            for (int u = 1; u <= n; u++) {
                int mid = up[k - 1][u];
                up[k][u] = up[k - 1][mid];
                auto merged = merge_pair({mx1[k - 1][u], mx2[k - 1][u]},
                                         {mx1[k - 1][mid], mx2[k - 1][mid]});
                mx1[k][u] = merged.first;
                mx2[k][u] = merged.second;
            }
        }
    }

    pair<long long, long long> path_max_two(int a, int b) {
        pair<long long, long long> ans = {NEG, NEG};

        if (depth[a] < depth[b]) swap(a, b);
        int diff = depth[a] - depth[b];
        for (int k = 0; k < lg; k++) {
            if (diff >> k & 1) {
                ans = merge_pair(ans, {mx1[k][a], mx2[k][a]});
                a = up[k][a];
            }
        }

        if (a == b) return ans;

        for (int k = lg - 1; k >= 0; k--) {
            if (up[k][a] != up[k][b]) {
                ans = merge_pair(ans, {mx1[k][a], mx2[k][a]});
                ans = merge_pair(ans, {mx1[k][b], mx2[k][b]});
                a = up[k][a];
                b = up[k][b];
            }
        }

        ans = merge_pair(ans, {mx1[0][a], mx2[0][a]});
        ans = merge_pair(ans, {mx1[0][b], mx2[0][b]});
        return ans;
    }

    long long solve() {
        sort(edges.begin(), edges.end());
        DSU dsu(n);

        long long mst = 0;
        int cnt = 0;
        for (auto& e : edges) {
            if (!dsu.merge(e.u, e.v)) continue;
            e.used = true;
            mst += e.w;
            cnt++;
            tree[e.u].push_back({e.v, e.w});
            tree[e.v].push_back({e.u, e.w});
        }

        if (cnt != n - 1) return -1;

        build_lca();

        long long ans = INF;
        for (const auto& e : edges) {
            if (e.used) continue;
            auto [largest, second_largest] = path_max_two(e.u, e.v);

            long long removed = NEG;
            if (largest < e.w) removed = largest;
            else if (second_largest < e.w) removed = second_largest;

            if (removed != NEG) {
                ans = min(ans, mst + e.w - removed);
            }
        }

        return ans == INF ? -1 : ans;
    }
};

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n, m;
    cin >> n >> m;

    StrictSecondMST solver(n);
    for (int i = 1; i <= m; i++) {
        int u, v;
        long long w;
        cin >> u >> v >> w;
        solver.add_edge(u, v, w, i);
    }

    cout << solver.solve() << '\n';
    return 0;
}
