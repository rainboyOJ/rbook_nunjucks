#include <algorithm>
#include <vector>

const int maxn = 1e6 + 5;
using Graph = std::vector<int>;
Graph tree[maxn];  // 全局邻接表数组：直接向 tree[u] 加边

// 如何使用：
// 1. 读入边：tree[u].push_back(v); tree[v].push_back(u);
// 2. TreeCentroid tc(n);
// 3. vector<int> cs = tc.find_centroids();  // 返回所有重心，编号升序

// 求树的所有重心。
// 重心：删除该点后，剩下的每个连通块大小都不超过 n/2。
struct TreeCentroid {
    int n;
    std::vector<int> sz;   // sz[u] = u 的子树大小
    std::vector<int> ans;  // 答案：所有重心，按编号升序
    int best;              // 最小的 B(u)：删除 u 后最大的连通块大小

    explicit TreeCentroid(int n) : n(n), best(n), sz(n + 1) {}

    // 返回所有重心（编号升序）
    std::vector<int> find_centroids(int root = 1) {
        best = n;
        ans.clear();
        dfs(root, 0);
        std::sort(ans.begin(), ans.end());
        return ans;
    }

    // 统计子树大小，同时计算每个点的 B(u) = 删除 u 后最大的连通块大小
    void dfs(int u, int parent) {
        sz[u] = 1;
        int mx = 0;  // B(u)：先看各儿子子树

        for (int v : tree[u]) {
            if (v == parent) continue;
            dfs(v, u);
            sz[u] += sz[v];
            mx = std::max(mx, sz[v]);
        }

        // 父亲方向也是一块：整棵树减去 u 的子树
        mx = std::max(mx, n - sz[u]);

        // 记录 B(u) 最小的点（可能不止一个）
        if (mx < best) {
            best = mx;
            ans = {u};
        } else if (mx == best) {
            ans.push_back(u);
        }
    }
};
