#include <bits/stdc++.h>
using namespace std;

struct Edge {
    int to;
    long long w;
};

struct NegativeCycleSPFA {
    int n;
    vector<vector<Edge>> graph;
    vector<long long> dist;
    vector<int> relax_count;
    vector<bool> in_queue;

    explicit NegativeCycleSPFA(int n)
        : n(n),
          graph(n + 1),
          dist(n + 1, 0),
          relax_count(n + 1, 0),
          in_queue(n + 1, false) {}

    void add_edge(int u, int v, long long w) {
        graph[u].push_back({v, w});
    }

    bool has_negative_cycle() {
        queue<int> q;
        for (int i = 1; i <= n; ++i) {
            q.push(i);
            in_queue[i] = true;
        }

        while (!q.empty()) {
            int u = q.front();
            q.pop();
            in_queue[u] = false;

            for (const auto& e : graph[u]) {
                int v = e.to;
                if (dist[v] > dist[u] + e.w) {
                    dist[v] = dist[u] + e.w;
                    relax_count[v] = relax_count[u] + 1;
                    if (relax_count[v] >= n) return true;
                    if (!in_queue[v]) {
                        q.push(v);
                        in_queue[v] = true;
                    }
                }
            }
        }

        return false;
    }
};

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n, m;
    cin >> n >> m;

    NegativeCycleSPFA spfa(n);
    for (int i = 0; i < m; ++i) {
        int u, v;
        long long w;
        cin >> u >> v >> w;
        spfa.add_edge(u, v, w);
    }

    cout << (spfa.has_negative_cycle() ? "Yes" : "No") << '\n';
    return 0;
}
