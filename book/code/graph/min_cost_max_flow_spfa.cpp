#include <bits/stdc++.h>
using namespace std;

struct MinCostMaxFlow {
    struct Edge {
        int to;
        int rev;
        int cap;
        int cost;
    };

    static constexpr int INF = 1e9;
    int n;
    vector<vector<Edge>> g;
    vector<int> dist, prev_v, prev_e;

    explicit MinCostMaxFlow(int n) : n(n), g(n + 1), dist(n + 1), prev_v(n + 1), prev_e(n + 1) {}

    void add_edge(int from, int to, int cap, int cost) {
        Edge forward{to, (int)g[to].size(), cap, cost};
        Edge backward{from, (int)g[from].size(), 0, -cost};
        g[from].push_back(forward);
        g[to].push_back(backward);
    }

    bool spfa(int s, int t) {
        fill(dist.begin(), dist.end(), INF);
        vector<bool> in_queue(n + 1, false);
        queue<int> q;

        dist[s] = 0;
        in_queue[s] = true;
        q.push(s);

        while (!q.empty()) {
            int u = q.front();
            q.pop();
            in_queue[u] = false;

            for (int i = 0; i < (int)g[u].size(); ++i) {
                const Edge &e = g[u][i];
                if (e.cap <= 0) continue;
                if (dist[e.to] > dist[u] + e.cost) {
                    dist[e.to] = dist[u] + e.cost;
                    prev_v[e.to] = u;
                    prev_e[e.to] = i;
                    if (!in_queue[e.to]) {
                        in_queue[e.to] = true;
                        q.push(e.to);
                    }
                }
            }
        }

        return dist[t] != INF;
    }

    pair<int, int> min_cost_max_flow(int s, int t) {
        int flow = 0;
        int cost = 0;

        while (spfa(s, t)) {
            int pushed = INF;
            for (int v = t; v != s; v = prev_v[v]) {
                const Edge &e = g[prev_v[v]][prev_e[v]];
                pushed = min(pushed, e.cap);
            }

            flow += pushed;
            cost += pushed * dist[t];

            for (int v = t; v != s; v = prev_v[v]) {
                Edge &e = g[prev_v[v]][prev_e[v]];
                e.cap -= pushed;
                g[v][e.rev].cap += pushed;
            }
        }

        return {flow, cost};
    }
};

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n, m, s, t;
    cin >> n >> m >> s >> t;

    MinCostMaxFlow solver(n);
    for (int i = 0; i < m; ++i) {
        int u, v, cap, cost;
        cin >> u >> v >> cap >> cost;
        solver.add_edge(u, v, cap, cost);
    }

    auto [flow, cost] = solver.min_cost_max_flow(s, t);
    cout << flow << ' ' << cost << '\n';
    return 0;
}
