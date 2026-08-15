#include <algorithm>
#include <vector>

struct TreeCentroid {
    int n;
    int best_balance;
    std::vector<std::vector<int>> graph;
    std::vector<int> subtree_size;
    std::vector<int> centroids;

    explicit TreeCentroid(int n)
        : n(n), best_balance(n), graph(n + 1), subtree_size(n + 1) {}

    void add_edge(int u, int v) {
        graph[u].push_back(v);
        graph[v].push_back(u);
    }

    std::vector<int> find_centroids(int root = 1) {
        best_balance = n;
        centroids.clear();
        dfs(root, 0);
        std::sort(centroids.begin(), centroids.end());
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
            largest_part = std::max(largest_part, subtree_size[v]);
        }

        // Removing u also leaves the part above u.
        largest_part = std::max(largest_part, n - subtree_size[u]);

        if (largest_part < best_balance) {
            best_balance = largest_part;
            centroids = {u};
        } else if (largest_part == best_balance) {
            centroids.push_back(u);
        }
    }
};
