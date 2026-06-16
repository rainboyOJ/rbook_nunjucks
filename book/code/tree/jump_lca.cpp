#include <bits/stdc++.h>
using namespace std;

const int MAXN = 200000 + 5;
const int LOG = 20;  // 2^20 > 1e6, 按题目规模调整

struct BinaryLCA {
    int n;
    vector<int> g[MAXN];
    int depth[MAXN];
    int up[MAXN][LOG + 1];  // up[u][j] 表示 u 的 2^j 级祖先

    void init(int n_) {
        n = n_;
        for (int i = 1; i <= n; ++i) {
            g[i].clear();
            depth[i] = 0;
            for (int j = 0; j <= LOG; ++j) up[i][j] = 0;
        }
    }

    void add_edge(int u, int v) {
        g[u].push_back(v);
        g[v].push_back(u);
    }

    void dfs(int u, int fa) {
        up[u][0] = fa;
        depth[u] = depth[fa] + 1;
        for (int j = 1; j <= LOG; ++j) {
            up[u][j] = up[up[u][j - 1]][j - 1];
        }
        for (int v : g[u]) {
            if (v == fa) continue;
            dfs(v, u);
        }
    }

    void build(int root = 1) {
        depth[0] = 0;
        dfs(root, 0);
    }

    int kth_ancestor(int u, int k) const {
        for (int j = 0; j <= LOG; ++j) {
            if (k & (1 << j)) u = up[u][j];
        }
        return u;
    }

    int lca(int a, int b) const {
        if (depth[a] < depth[b]) swap(a, b);

        a = kth_ancestor(a, depth[a] - depth[b]);
        if (a == b) return a;

        for (int j = LOG; j >= 0; --j) {
            if (up[a][j] != up[b][j]) {
                a = up[a][j];
                b = up[b][j];
            }
        }
        return up[a][0];
    }

    int dist(int a, int b) const {
        int c = lca(a, b);
        return depth[a] + depth[b] - 2 * depth[c];
    }
};
