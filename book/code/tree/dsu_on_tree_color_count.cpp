#include <bits/stdc++.h>
using namespace std;

struct DsuOnTree {
    int n = 0;
    vector<vector<int>> graph;
    vector<int> color;
    vector<int> subtree_size;
    vector<int> heavy_child;
    vector<int> answer;
    vector<int> color_count;
    int max_count = 0;
    int sum_color = 0;
    int big_child = 0;

    DsuOnTree(int n, int max_color) : n(n) {
        graph.assign(n + 1, {});
        color.assign(n + 1, 0);
        subtree_size.assign(n + 1, 0);
        heavy_child.assign(n + 1, 0);
        answer.assign(n + 1, 0);
        color_count.assign(max_color + 1, 0);
    }

    void add_edge(int u, int v) {
        graph[u].push_back(v);
        graph[v].push_back(u);
    }

    void dfs_size(int u, int parent) {
        subtree_size[u] = 1;
        for (int v : graph[u]) {
            if (v == parent) continue;
            dfs_size(v, u);
            subtree_size[u] += subtree_size[v];
            if (subtree_size[v] > subtree_size[heavy_child[u]]) {
                heavy_child[u] = v;
            }
        }
    }

    void add_color(int c, int delta) {
        color_count[c] += delta;
        if (delta > 0) {
            if (color_count[c] > max_count) {
                max_count = color_count[c];
                sum_color = c;
            } else if (color_count[c] == max_count) {
                sum_color += c;
            }
        }
    }

    void add_subtree(int u, int parent, int delta) {
        add_color(color[u], delta);
        for (int v : graph[u]) {
            if (v == parent || v == big_child) continue;
            add_subtree(v, u, delta);
        }
    }

    void reset_state() {
        fill(color_count.begin(), color_count.end(), 0);
        max_count = 0;
        sum_color = 0;
    }

    void dfs_solve(int u, int parent, bool keep) {
        for (int v : graph[u]) {
            if (v == parent || v == heavy_child[u]) continue;
            dfs_solve(v, u, false);
        }

        if (heavy_child[u]) {
            dfs_solve(heavy_child[u], u, true);
            big_child = heavy_child[u];
        }

        add_subtree(u, parent, 1);
        big_child = 0;
        answer[u] = sum_color;

        if (!keep) {
            add_subtree(u, parent, -1);
            reset_state();
        }
    }
};

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n;
    cin >> n;

    vector<int> input_color(n + 1);
    int max_color = 0;
    for (int i = 1; i <= n; i++) {
        cin >> input_color[i];
        max_color = max(max_color, input_color[i]);
    }

    DsuOnTree solver(n, max_color);
    for (int i = 1; i <= n; i++) {
        solver.color[i] = input_color[i];
    }

    for (int i = 1; i < n; i++) {
        int u, v;
        cin >> u >> v;
        solver.add_edge(u, v);
    }

    solver.dfs_size(1, 0);
    solver.dfs_solve(1, 0, true);

    for (int i = 1; i <= n; i++) {
        if (i > 1) cout << ' ';
        cout << solver.answer[i];
    }
    cout << '\n';

    return 0;
}
