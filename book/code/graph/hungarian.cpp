#include <bits/stdc++.h>
using namespace std;

// 二分图最大匹配：左部点编号 1..n，右部点编号 1..m。
// dfs(u) 尝试给左部点 u 找到一个右部匹配点。
struct Hungarian {
    int n, m;
    vector<vector<int>> g;
    vector<int> match;
    vector<int> vis;

    Hungarian(int n, int m) : n(n), m(m), g(n + 1), match(m + 1, 0), vis(m + 1, 0) {}

    void add_edge(int u, int v) {
        if (u < 1 || u > n || v < 1 || v > m) return;
        g[u].push_back(v);
    }

    bool dfs(int u) {
        for (int v : g[u]) {
            if (vis[v]) continue;
            vis[v] = 1;

            // v 为空，或 v 当前匹配的左部点可以被挪到别处。
            if (match[v] == 0 || dfs(match[v])) {
                match[v] = u;
                return true;
            }
        }
        return false;
    }

    int max_matching() {
        int ans = 0;
        fill(match.begin(), match.end(), 0);

        for (int u = 1; u <= n; u++) {
            fill(vis.begin(), vis.end(), 0);
            if (dfs(u)) ans++;
        }
        return ans;
    }
};

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n, m, e;
    cin >> n >> m >> e;

    Hungarian solver(n, m);
    for (int i = 0; i < e; i++) {
        int u, v;
        cin >> u >> v;
        solver.add_edge(u, v);
    }

    cout << solver.max_matching() << '\n';
    return 0;
}
