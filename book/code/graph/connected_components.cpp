#include <bits/stdc++.h>
using namespace std;

struct Graph {
    int n;
    vector<vector<int>> adj;
    vector<int> component;
    int component_count = 0;

    explicit Graph(int n) : n(n), adj(n + 1), component(n + 1, 0) {}

    void add_edge(int u, int v) {
        adj[u].push_back(v);
        adj[v].push_back(u);
    }

    void dfs(int u, int id) {
        component[u] = id;
        for (int v : adj[u]) {
            if (component[v]) continue;
            dfs(v, id);
        }
    }

    void find_components() {
        for (int u = 1; u <= n; u++) {
            if (component[u]) continue;
            component_count++;
            dfs(u, component_count);
        }
    }
};

int main() {
    int n, m;
    cin >> n >> m;

    Graph graph(n);
    for (int i = 0; i < m; i++) {
        int u, v;
        cin >> u >> v;
        graph.add_edge(u, v);
    }

    graph.find_components();

    cout << graph.component_count << "\n";
    for (int u = 1; u <= n; u++) {
        if (u > 1) cout << ' ';
        cout << graph.component[u];
    }
    cout << "\n";

    return 0;
}
