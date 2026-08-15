#include <bits/stdc++.h>
using namespace std;

struct TreeCentroid {
    int n;
    int best_balance;
    vector<vector<int>> graph;
    vector<int> subtree_size;
    vector<int> centroids;

    explicit TreeCentroid(int n)
        : n(n), best_balance(n), graph(n + 1), subtree_size(n + 1) {}

    void add_edge(int u, int v) {
        graph[u].push_back(v);
        graph[v].push_back(u);
    }

    vector<int> find_centroids(int root = 1) {
        best_balance = n;
        centroids.clear();
        dfs(root, 0);
        sort(centroids.begin(), centroids.end());
        return centroids;
    }

private:
    void dfs(int u, int parent) {
        subtree_size[u] = 1;
        int largest_part = 0;

        for (int v : graph[u]) {
            if (v == parent) continue;
            dfs(v, u);
            subtree_size[u] += subtree_size[v];
            largest_part = max(largest_part, subtree_size[v]);
        }

        largest_part = max(largest_part, n - subtree_size[u]);

        if (largest_part < best_balance) {
            best_balance = largest_part;
            centroids = {u};
        } else if (largest_part == best_balance) {
            centroids.push_back(u);
        }
    }
};

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n;
    cin >> n;

    TreeCentroid tree(n);
    for (int i = 1; i < n; ++i) {
        int u, v;
        cin >> u >> v;
        tree.add_edge(u, v);
    }

    vector<int> answer = tree.find_centroids();
    cout << answer.size() << '\n';
    for (int i = 0; i < static_cast<int>(answer.size()); ++i) {
        if (i > 0) cout << ' ';
        cout << answer[i];
    }
    cout << '\n';
    return 0;
}
