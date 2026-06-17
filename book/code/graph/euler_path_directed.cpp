#include <bits/stdc++.h>
using namespace std;

// Directed Eulerian path/circuit by Hierholzer's algorithm.
// Input:
//   n m
//   m lines: u v, meaning u -> v
// Output:
//   one Eulerian vertex sequence, or NO if it does not exist.

struct Edge {
    int to;
    int id;
};

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n, m;
    if (!(cin >> n >> m)) return 0;

    vector<vector<Edge>> g(n + 1);
    vector<vector<int>> weak(n + 1);
    vector<int> indeg(n + 1, 0), outdeg(n + 1, 0);

    for (int id = 1; id <= m; ++id) {
        int u, v;
        cin >> u >> v;
        g[u].push_back({v, id});
        weak[u].push_back(v);
        weak[v].push_back(u);
        ++outdeg[u];
        ++indeg[v];
    }

    int first = -1;
    for (int i = 1; i <= n; ++i) {
        if (indeg[i] + outdeg[i] > 0) {
            first = i;
            break;
        }
    }

    if (first == -1) {
        cout << '\n';
        return 0;
    }

    vector<char> vis(n + 1, 0);
    stack<int> st;
    st.push(first);
    vis[first] = 1;
    while (!st.empty()) {
        int u = st.top();
        st.pop();
        for (int v : weak[u]) {
            if (!vis[v]) {
                vis[v] = 1;
                st.push(v);
            }
        }
    }

    for (int i = 1; i <= n; ++i) {
        if (indeg[i] + outdeg[i] > 0 && !vis[i]) {
            cout << "NO\n";
            return 0;
        }
    }

    int start_count = 0, end_count = 0;
    int start = first;
    for (int i = 1; i <= n; ++i) {
        if (outdeg[i] == indeg[i] + 1) {
            ++start_count;
            start = i;
        } else if (indeg[i] == outdeg[i] + 1) {
            ++end_count;
        } else if (indeg[i] != outdeg[i]) {
            cout << "NO\n";
            return 0;
        }
    }

    bool has_path = (start_count == 1 && end_count == 1);
    bool has_circuit = (start_count == 0 && end_count == 0);
    if (!has_path && !has_circuit) {
        cout << "NO\n";
        return 0;
    }

    vector<char> used(m + 1, 0);
    vector<int> iter(n + 1, 0), stack_path, path;
    stack_path.push_back(start);

    while (!stack_path.empty()) {
        int u = stack_path.back();
        while (iter[u] < (int)g[u].size() && used[g[u][iter[u]].id]) {
            ++iter[u];
        }

        if (iter[u] == (int)g[u].size()) {
            path.push_back(u);
            stack_path.pop_back();
        } else {
            Edge e = g[u][iter[u]++];
            used[e.id] = 1;
            stack_path.push_back(e.to);
        }
    }

    if ((int)path.size() != m + 1) {
        cout << "NO\n";
        return 0;
    }

    reverse(path.begin(), path.end());
    for (int i = 0; i < (int)path.size(); ++i) {
        if (i) cout << ' ';
        cout << path[i];
    }
    cout << '\n';

    return 0;
}
