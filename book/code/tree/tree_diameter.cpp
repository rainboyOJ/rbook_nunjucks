#include <bits/stdc++.h>
using namespace std;

struct TreeDiameter {
    using ll = long long;

    int n = 0;
    vector<vector<pair<int, ll>>> graph;

    TreeDiameter(int n = 0) {
        init(n);
    }

    void init(int node_count) {
        n = node_count;
        graph.assign(n + 1, {});
    }

    void add_edge(int u, int v, ll w) {
        graph[u].push_back({v, w});
        graph[v].push_back({u, w});
    }

    pair<int, ll> farthest_from(int start) const {
        vector<ll> dist(n + 1, -1);
        stack<int> st;
        st.push(start);
        dist[start] = 0;

        int farthest = start;
        while (!st.empty()) {
            int u = st.top();
            st.pop();

            if (dist[u] > dist[farthest]) {
                farthest = u;
            }

            for (auto [v, w] : graph[u]) {
                if (dist[v] != -1) continue;
                dist[v] = dist[u] + w;
                st.push(v);
            }
        }

        return {farthest, dist[farthest]};
    }

    tuple<int, int, ll> solve() const {
        auto [left, _] = farthest_from(1);
        auto [right, diameter] = farthest_from(left);
        return {left, right, diameter};
    }
};

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n;
    cin >> n;

    TreeDiameter solver(n);
    for (int i = 1; i < n; i++) {
        int u, v;
        long long w;
        cin >> u >> v >> w;
        solver.add_edge(u, v, w);
    }

    auto [u, v, diameter] = solver.solve();
    cout << diameter << '\n';
    return 0;
}
