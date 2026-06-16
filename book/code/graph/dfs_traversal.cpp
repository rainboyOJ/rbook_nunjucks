#include <bits/stdc++.h>
using namespace std;

struct Graph {
    int n;
    vector<vector<int>> adj;
    vector<int> visited;
    vector<int> order;

    explicit Graph(int n) : n(n), adj(n + 1), visited(n + 1, 0) {}

    void add_edge(int u, int v) {
        adj[u].push_back(v);
        adj[v].push_back(u);
    }

    void dfs(int u) {
        visited[u] = 1;
        order.push_back(u);

        for (int v : adj[u]) {
            if (visited[v]) continue;
            dfs(v);
        }
    }
};

int main() {
    int n, m, start;
    cin >> n >> m >> start;

    Graph graph(n);
    for (int i = 0; i < m; i++) {
        int u, v;
        cin >> u >> v;
        graph.add_edge(u, v);
    }

    for (int u = 1; u <= n; u++) {
        sort(graph.adj[u].begin(), graph.adj[u].end());
    }

    graph.dfs(start);

    for (int i = 0; i < (int)graph.order.size(); i++) {
        if (i) cout << ' ';
        cout << graph.order[i];
    }
    cout << "\n";

    return 0;
}
