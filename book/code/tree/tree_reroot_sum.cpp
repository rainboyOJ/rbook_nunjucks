#include <algorithm>
#include <vector>

const int maxn = 1e6 + 5;
using ll = long long;

// 全局邻接表：无向无权树，向 tree[u] 加边 tree[u].push_back(v) 和 tree[v].push_back(u)
using Graph = std::vector<int>;
Graph tree[maxn];
ll weight[maxn];  // weight[u]：点权；无权版全部填 1

// 换根 DP（求和型）：求每个点作为根时的加权距离和 Σ weight[x] * dist(x, u)。
// 用法：
// 1. 读入 n 与边，填好 weight[1..n]；
// 2. TreeRerootSum solver(n);
// 3. const std::vector<ll>& ans = solver.solve(1);  // ans[u] 就是以 u 为根时的答案
struct TreeRerootSum {
    int n;
    int root;               // 第一次 DFS 用的根
    ll total;               // 全树点权和
    std::vector<ll> sub;    // sub[u]：u 子树内的点权和
    std::vector<ll> ans;    // ans[u]：以 u 为根时的加权距离和

    explicit TreeRerootSum(int n) : n(n), root(1), total(0), sub(n + 1), ans(n + 1) {}

    const std::vector<ll>& solve(int start = 1) {
        root = start;
        total = 0;
        for (int u = 1; u <= n; u++) total += weight[u];
        std::fill(ans.begin(), ans.end(), 0);
        dfs_sum(root, 0, 0);
        dfs_reroot(root, 0);
        return ans;
    }

    // 第一次 DFS：统计子树点权和，同时把根的答案直接按深度算出来
    void dfs_sum(int u, int parent, int depth) {
        sub[u] = weight[u];
        ans[root] += weight[u] * depth;

        for (int v : tree[u]) {
            if (v == parent) continue;
            dfs_sum(v, u, depth + 1);
            sub[u] += sub[v];
        }
    }

    // 第二次 DFS：沿父子边递推。根从 u 换到儿子 v 时，
    // v 子树内 sub[v] 个点各少走一步，其余 total - sub[v] 个点各多走一步
    void dfs_reroot(int u, int parent) {
        for (int v : tree[u]) {
            if (v == parent) continue;
            ans[v] = ans[u] + total - 2 * sub[v];
            dfs_reroot(v, u);
        }
    }
};
