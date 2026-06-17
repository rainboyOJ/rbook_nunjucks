#include <bits/stdc++.h>
using namespace std;

struct TopologicalSort {
    int n;
    vector<vector<int>> graph;
    vector<int> indeg;

    explicit TopologicalSort(int n) : n(n), graph(n + 1), indeg(n + 1, 0) {}

    void add_edge(int u, int v) {
        graph[u].push_back(v);
        indeg[v]++;
    }

    vector<int> kahn() {
        queue<int> q;
        vector<int> deg = indeg;
        vector<int> order;

        for (int i = 1; i <= n; i++) {
            if (deg[i] == 0) q.push(i);
        }

        while (!q.empty()) {
            int u = q.front();
            q.pop();
            order.push_back(u);

            for (int v : graph[u]) {
                deg[v]--;
                if (deg[v] == 0) q.push(v);
            }
        }

        return order;
    }
};

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n, m;
    cin >> n >> m;

    TopologicalSort solver(n);
    for (int i = 0; i < m; i++) {
        int u, v;
        cin >> u >> v;
        solver.add_edge(u, v);
    }

    vector<int> order = solver.kahn();
    if ((int)order.size() != n) {
        cout << "Cycle\n";
        return 0;
    }

    for (int i = 0; i < n; i++) {
        if (i) cout << ' ';
        cout << order[i];
    }
    cout << '\n';
    return 0;
}
