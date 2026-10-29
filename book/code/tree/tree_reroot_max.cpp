#include <algorithm>
#include <vector>

const int maxn = 1e6 + 5;
using ll = long long;

// 全局邻接表：无向无权树，向 tree[u] 加边 tree[u].push_back(v) 和 tree[v].push_back(u)
using Graph = std::vector<int>;
Graph tree[maxn];

// 换根 DP（最值型）：求每个点到其它点的最远距离（偏心距）。
// 偏心距最小的点就是树的中心，所以这也是按换根求树的中心的办法。
// 用法：
// 1. 读入 n 与边；
// 2. TreeRerootMax solver(n);
// 3. const std::vector<ll>& ans = solver.solve(1);  // ans[u] 就是 u 的偏心距
struct TreeRerootMax {
    int n;
    int root;
    std::vector<ll> down;   // down[u]：u 子树内离 u 最远的距离
    std::vector<ll> up;     // up[u]：从 u 往父亲方向走能到的最远距离（不经过 u 的子树）
    std::vector<ll> ans;    // ans[u] = max(down[u], up[u])

    explicit TreeRerootMax(int n) : n(n), root(1), down(n + 1), up(n + 1), ans(n + 1) {}

    const std::vector<ll>& solve(int start = 1) {
        root = start;
        std::fill(down.begin(), down.end(), 0);
        std::fill(up.begin(), up.end(), 0);
        dfs_down(root, 0);
        dfs_up(root, 0);
        for (int u = 1; u <= n; u++) ans[u] = std::max(down[u], up[u]);
        return ans;
    }

    // 第一次 DFS：后序求 down[u]
    void dfs_down(int u, int parent) {
        down[u] = 0;
        for (int v : tree[u]) {
            if (v == parent) continue;
            dfs_down(v, u);
            if (down[v] + 1 > down[u]) down[u] = down[v] + 1;
        }
    }

    // 第二次 DFS：前序把"父亲方向"的信息传给儿子。
    // 对儿子 v 来说，向上的最远路 = 先走到 u，再从 u 选一条不经过 v 的最长路。
    // 所以要记 u 的子方向最长和次长，v 贡献了最长时改用次长。
    void dfs_up(int u, int parent) {
        ll best1 = -1;  // 子树方向最长的 down[儿子] + 1
        ll best2 = -1;  // 与 best1 来自不同儿子的次长

        for (int v : tree[u]) {
            if (v == parent) continue;
            ll candidate = down[v] + 1;
            if (candidate > best1) {
                best2 = best1;
                best1 = candidate;
            } else if (candidate > best2) {
                best2 = candidate;
            }
        }

        for (int v : tree[u]) {
            if (v == parent) continue;

            ll outside;  // 从 u 出发、不进入 v 子树能走的最远距离
            if (down[v] + 1 == best1) {
                outside = best2;  // v 提供了最长，只能改用次长
            } else {
                outside = best1;
            }
            if (up[u] > outside) outside = up[u];
            if (outside < 0) outside = 0;  // 实在没地方去，就停在 u

            up[v] = outside + 1;  // 从 v 先走到 u，再走 outside 步
        }

        for (int v : tree[u]) {
            if (v == parent) continue;
            dfs_up(v, u);
        }
    }
};
