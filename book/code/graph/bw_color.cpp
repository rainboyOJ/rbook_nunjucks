#include <bits/stdc++.h>
using namespace std;

struct BipartiteChecker {
    int n = 0;
    vector<vector<int>> graph;
    vector<int> color; // 0: uncolored, 1 and 2: two sides.

    BipartiteChecker(int n = 0) {
        init(n);
    }

    void init(int node_count) {
        n = node_count;
        graph.assign(n + 1, {});
        color.assign(n + 1, 0);
    }

    void add_edge(int u, int v) {
        graph[u].push_back(v);
        graph[v].push_back(u);
    }

    bool bfs_color(int start) {
        queue<int> q;
        color[start] = 1;
        q.push(start);

        while (!q.empty()) {
            int u = q.front();
            q.pop();

            for (int v : graph[u]) {
                if (color[v] == 0) {
                    color[v] = 3 - color[u];
                    q.push(v);
                } else if (color[v] == color[u]) {
                    return false;
                }
            }
        }

        return true;
    }

    bool is_bipartite() {
        for (int i = 1; i <= n; i++) {
            if (color[i] == 0 && !bfs_color(i)) {
                return false;
            }
        }
        return true;
    }
};

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n, m;
    cin >> n >> m;

    BipartiteChecker checker(n);
    for (int i = 0; i < m; i++) {
        int u, v;
        cin >> u >> v;
        checker.add_edge(u, v);
    }

    cout << (checker.is_bipartite() ? "Yes" : "No") << '\n';
    return 0;
}
