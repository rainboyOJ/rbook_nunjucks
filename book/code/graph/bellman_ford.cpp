#include <bits/stdc++.h>
using namespace std;

const long long INF = (1LL << 60);

struct Edge {
    int from;
    int to;
    long long weight;
};

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n, m, source;
    cin >> n >> m >> source;

    vector<Edge> edges;
    edges.reserve(m);
    for (int i = 0; i < m; i++) {
        int u, v;
        long long w;
        cin >> u >> v >> w;
        edges.push_back({u, v, w});
    }

    vector<long long> dist(n + 1, INF);
    dist[source] = 0;

    for (int round = 1; round <= n - 1; round++) {
        bool changed = false;
        for (const auto &edge : edges) {
            if (dist[edge.from] == INF) continue;
            if (dist[edge.to] > dist[edge.from] + edge.weight) {
                dist[edge.to] = dist[edge.from] + edge.weight;
                changed = true;
            }
        }
        if (!changed) break;
    }

    bool has_negative_cycle = false;
    for (const auto &edge : edges) {
        if (dist[edge.from] == INF) continue;
        if (dist[edge.to] > dist[edge.from] + edge.weight) {
            has_negative_cycle = true;
            break;
        }
    }

    if (has_negative_cycle) {
        cout << "Negative cycle\n";
        return 0;
    }

    for (int i = 1; i <= n; i++) {
        if (i > 1) cout << ' ';
        if (dist[i] == INF) cout << "INF";
        else cout << dist[i];
    }
    cout << '\n';

    return 0;
}
