#include <algorithm>
#include <vector>
#include <cstring>

const int maxn = 1e6 + 5;
using Graph = std::vector<int>;
Graph tree[maxn];  // 全局邻接表数组：直接向 tree[u] 加无权边

// 如何使用：
// 1. 读入边：tree[u].push_back(v); tree[v].push_back(u);
// 2. BinaryLCA lca(n);
// 3. lca.build(root);               // 构建/预处理 DFS 深度和倍增表（默认 root = 1）
// 4. int p = lca.query(a, b);       // 查询 a 和 b 的 LCA（或使用 lca.lca(a, b)）
// 5. int d = lca.dist(a, b);        // 查询 a 和 b 在树上的距离

// 倍增算法求最近公共祖先（LCA）与树上距离
struct BinaryLCA {
    constexpr static int maxn = 1e6 + 5;
    constexpr static int max_log = 20;  // 支持最多 2^20 = 1,048,576 个节点
    int n;
    int depth[maxn];
    int up[maxn][max_log]; // up[u][j] 表示节点 u 的 2^j 级祖先

    explicit BinaryLCA(int n = 0) : n(n) {
        memset(up, 0, sizeof(up));
        for (int i = 0; i <= n; ++i) depth[i] = 0;
    }

    // DFS 预处理每个节点的深度 depth 以及 2^j 级祖先表 up
    void dfs(int u, int fa) {
        up[u][0] = fa;
        depth[u] = depth[fa] + 1;
        for (int j = 1; j < max_log; ++j) {
            up[u][j] = up[up[u][j - 1]][j - 1];
        }
        for (int v : tree[u]) {
            if (v == fa) continue;
            dfs(v, u);
        }
    }

    // 构建/预处理 LCA 倍增表，默认以 root 为根节点
    void build(int root = 1) {
        depth[0] = 0;
        dfs(root, 0);
    }

    // 查询节点 u 的第 k 级祖先（向上跳 k 步）
    int kth_ancestor(int u, int k) {
        for (int j = 0; j < max_log; ++j) {
            if (k & (1 << j)) u = up[u][j];
        }
        return u;
    }

    // 查询节点 a 和节点 b 的最近公共祖先（LCA）
    int lca(int a, int b) {
        //保证 a  是较深节点
        if (depth[a] < depth[b]) std::swap(a, b);

        a = kth_ancestor(a, depth[a] - depth[b]);
        if (a == b) return a;

        for (int j = max_log - 1; j >= 0; --j) {
            if (up[a][j] != up[b][j]) {
                a = up[a][j];
                b = up[b][j];
            }
        }
        return up[a][0];
    }


    // 计算节点 a 和节点 b 在树上的距离
    int dist(int a, int b) {
        int c = lca(a, b);
        return depth[a] + depth[b] - 2 * depth[c];
    }
};
