#include <bits/stdc++.h>
using namespace std;

struct DFSOrder {
    int n;
    int timer = 0;
    vector<vector<int>> tree;
    vector<int> in, out, parent, depth, order;

    explicit DFSOrder(int n)
        : n(n),
          tree(n + 1),
          in(n + 1),
          out(n + 1),
          parent(n + 1),
          depth(n + 1),
          order(n + 1) {}

    void add_edge(int u, int v) {
        tree[u].push_back(v);
        tree[v].push_back(u);
    }

    void dfs(int u, int fa) {
        parent[u] = fa;
        depth[u] = depth[fa] + 1;
        in[u] = ++timer;
        order[timer] = u;

        for (int v : tree[u]) {
            if (v == fa) continue;
            dfs(v, u);
        }

        out[u] = timer;
    }

    bool is_ancestor(int u, int v) const {
        return in[u] <= in[v] && out[v] <= out[u];
    }
};

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n;
    cin >> n;

    DFSOrder solver(n);
    for (int i = 1; i < n; i++) {
        int u, v;
        cin >> u >> v;
        solver.add_edge(u, v);
    }

    solver.dfs(1, 0);

    for (int u = 1; u <= n; u++) {
        cout << u << ": " << solver.in[u] << ' ' << solver.out[u] << '\n';
    }

    return 0;
}
