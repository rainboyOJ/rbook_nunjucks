#include <algorithm>
#include <vector>

const int maxn = 1e6 + 5;
using ll = long long;
using Edge = struct { int to; ll w; };
using Graph = std::vector<Edge>;
Graph tree[maxn];  // 全局邻接表数组：直接向 tree[u] 加带权边

// 如何使用：
// 1. 读入带权边：tree[u].push_back({v, w}); tree[v].push_back({u, w});
// 2. TreeDiameterDP td(n);
// 3. td.solve(); cout << td.ans;  // 直径长度在 td.ans

// 树上 DP 求直径：对每个点统计"往下走的最长链"。
// 扫描 u 的儿子时，f[u] 表示已处理儿子提供的最长链；当前儿子 v
// 提供 f[v] + w。先用这两条链更新直径，再把当前链并入 f[u]。
struct TreeDiameterDP {
    int n;
    ll ans;              // 直径长度
    std::vector<ll> f;   // f[u] = 从 u 出发往下走的最长链长度

    explicit TreeDiameterDP(int n) : n(n), ans(0), f(n + 1) {}

    void solve() { dfs(1, 0); }

    // 后序遍历：处理儿子 v 时，f[u] 还是"已处理儿子的最长链"，
    // 先 ans = max(ans, f[u] + f[v] + w) 用两条链在 u 拼接，
    // 再 f[u] = max(f[u], f[v] + w) 把当前儿子的链并入 f[u]。
    void dfs(int u, int parent) {
        for (Edge e : tree[u]) {
            int v = e.to;
            ll w = e.w;
            if (v == parent) continue;
            dfs(v, u);

            ans = std::max(ans, f[u] + f[v] + w); // 过 u 的两条链拼接（f[u] 为 0 时就是单链）
            f[u] = std::max(f[u], f[v] + w);      // 更新过 u 的最长向下链
        }
    }
};
