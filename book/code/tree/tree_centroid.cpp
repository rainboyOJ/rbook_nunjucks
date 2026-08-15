#include <algorithm>
#include <vector>

using Graph = std::vector<std::vector<int>>;
Graph tree;  // 全局邻接表：使用前先 resize(n+1) 并加边

// 求树的所有重心。
// 重心：删除该点后，剩下的每个连通块大小都不超过 n/2。
struct TreeCentroid {
    int n;
    std::vector<int> subtree_size;               // subtree_size[u] = u 的子树大小
    std::vector<int> centroids;                  // 答案：所有重心，按编号升序
    Graph & tree;
    int best_balance;                            // 最小的 B(u)：删除 u 后最大的连通块大小

    explicit TreeCentroid(int n, Graph & tree) : n(n),tree(tree), best_balance(n), subtree_size(n + 1) {}

    // 返回所有重心（编号升序）
    std::vector<int> find_centroids(int root = 1) {
        best_balance = n;
        centroids.clear();
        dfs(root, 0);
        std::sort(centroids.begin(), centroids.end());
        return centroids;
    }

    // 统计子树大小，同时计算每个点的 B(u) = 删除 u 后最大的连通块大小
    void dfs(int u, int parent) {
        subtree_size[u] = 1;
        int largest_part = 0;  // B(u)：先看各儿子子树

        for (int v : tree[u]) {
            if (v == parent) continue;
            dfs(v, u);
            subtree_size[u] += subtree_size[v];
            largest_part = std::max(largest_part, subtree_size[v]);
        }

        // 父亲方向也是一块：整棵树减去 u 的子树
        largest_part = std::max(largest_part, n - subtree_size[u]);

        // 记录 B(u) 最小的点（可能不止一个）
        if (largest_part < best_balance) {
            best_balance = largest_part;
            centroids = {u};
        } else if (largest_part == best_balance) {
            centroids.push_back(u);
        }
    }
};

/* -- 使用方式
* tree.resize(n + 1);
* for (int i = 1; i < n; ++i) { int u, v; cin >> u >> v; tree[u].push_back(v); tree[v].push_back(u); }
* TreeCentroid tc(n);
* auto cs = tc.find_centroids();
*/
