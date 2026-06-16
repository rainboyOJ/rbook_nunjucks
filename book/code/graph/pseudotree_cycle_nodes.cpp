#include <bits/stdc++.h>
using namespace std;

struct PseudotreeCycle {
    int n;
    vector<vector<int>> graph;
    vector<int> degree;
    vector<bool> in_cycle;

    explicit PseudotreeCycle(int n)
        : n(n), graph(n + 1), degree(n + 1, 0), in_cycle(n + 1, true) {}

    void add_edge(int u, int v) {
        graph[u].push_back(v);
        graph[v].push_back(u);
        ++degree[u];
        ++degree[v];
    }

    vector<int> find_cycle_nodes() {
        queue<int> q;
        for (int i = 1; i <= n; ++i) {
            if (degree[i] <= 1) q.push(i);
        }

        while (!q.empty()) {
            int u = q.front();
            q.pop();
            if (!in_cycle[u]) continue;

            in_cycle[u] = false;
            for (int v : graph[u]) {
                if (!in_cycle[v]) continue;
                --degree[v];
                if (degree[v] == 1) q.push(v);
            }
        }

        vector<int> cycle_nodes;
        for (int i = 1; i <= n; ++i) {
            if (in_cycle[i]) cycle_nodes.push_back(i);
        }
        return cycle_nodes;
    }
};

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n, m;
    cin >> n >> m;

    PseudotreeCycle solver(n);
    for (int i = 0; i < m; ++i) {
        int u, v;
        cin >> u >> v;
        solver.add_edge(u, v);
    }

    vector<int> cycle_nodes = solver.find_cycle_nodes();
    for (int u : cycle_nodes) cout << u << ' ';
    cout << '\n';
    return 0;
}
