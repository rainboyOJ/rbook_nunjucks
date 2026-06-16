#include <bits/stdc++.h>
using namespace std;

struct Dinic {
    struct Edge {
        int to;
        int next;
        long long capacity;
    };

    vector<Edge> edges;
    vector<int> head;
    vector<int> level;
    vector<int> current;
    int node_count = 0;

    Dinic(int n = 0, int max_edges = 0) {
        init(n, max_edges);
    }

    void init(int n, int max_edges = 0) {
        node_count = n;
        edges.clear();
        edges.reserve(max_edges * 2 + 5);
        head.assign(n + 1, -1);
        level.resize(n + 1);
        current.resize(n + 1);
    }

    void add_edge(int u, int v, long long capacity) {
        edges.push_back({v, head[u], capacity});
        head[u] = (int)edges.size() - 1;
        edges.push_back({u, head[v], 0});
        head[v] = (int)edges.size() - 1;
    }

    bool bfs(int source, int sink) {
        fill(level.begin(), level.end(), -1);
        queue<int> q;
        level[source] = 0;
        q.push(source);

        while (!q.empty()) {
            int u = q.front();
            q.pop();

            for (int i = head[u]; i != -1; i = edges[i].next) {
                int v = edges[i].to;
                if (edges[i].capacity <= 0 || level[v] != -1) continue;
                level[v] = level[u] + 1;
                q.push(v);
            }
        }

        return level[sink] != -1;
    }

    long long dfs(int u, int sink, long long limit) {
        if (u == sink || limit == 0) return limit;

        long long flow = 0;
        for (int &i = current[u]; i != -1; i = edges[i].next) {
            int v = edges[i].to;
            if (edges[i].capacity <= 0 || level[v] != level[u] + 1) continue;

            long long pushed = dfs(v, sink, min(limit, edges[i].capacity));
            if (pushed == 0) continue;

            edges[i].capacity -= pushed;
            edges[i ^ 1].capacity += pushed;
            flow += pushed;
            limit -= pushed;
            if (limit == 0) break;
        }

        if (flow == 0) level[u] = -1;
        return flow;
    }

    long long max_flow(int source, int sink) {
        long long answer = 0;
        while (bfs(source, sink)) {
            current = head;
            while (true) {
                long long flow = dfs(source, sink, numeric_limits<long long>::max());
                if (flow == 0) break;
                answer += flow;
            }
        }
        return answer;
    }
};

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n, m, source, sink;
    cin >> n >> m >> source >> sink;

    Dinic dinic(n, m);
    for (int i = 0; i < m; i++) {
        int u, v;
        long long capacity;
        cin >> u >> v >> capacity;
        dinic.add_edge(u, v, capacity);
    }

    cout << dinic.max_flow(source, sink) << '\n';
    return 0;
}
