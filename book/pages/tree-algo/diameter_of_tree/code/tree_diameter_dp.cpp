#include <algorithm>
#include <iostream>
#include <vector>

const int maxn = 1e6 + 5;
using ll = long long;
using Edge = struct { int to; ll w; };
using Graph = std::vector<Edge>;
Graph tree[maxn];

// 树上 DP 求直径：扫描 u 的儿子时，用 f[u] 维护此前最长链，
// 再把当前儿子的链与它拼起来更新直径。
struct TreeDiameterDP {
    int n;
    ll ans;
    std::vector<ll> f; // f[u] = 从 u 出发往下走的最长链长度

    explicit TreeDiameterDP(int n) : n(n), ans(0), f(n + 1) {}

    void solve() { dfs(1, 0); }

    void dfs(int u, int parent) {
        for (Edge e : tree[u]) {
            int v = e.to;
            ll w = e.w;
            if (v == parent) continue;
            dfs(v, u);

            // 必须先更新答案，确保 f[u] 还不包含当前儿子 v。
            ans = std::max(ans, f[u] + f[v] + w);
            f[u] = std::max(f[u], f[v] + w);
        }
    }
};

int main() {
    std::ios::sync_with_stdio(false);
    std::cin.tie(nullptr);

    int n;
    std::cin >> n;

    for (int i = 1; i < n; ++i) {
        int u, v;
        ll w;
        std::cin >> u >> v >> w;
        tree[u].push_back({v, w});
        tree[v].push_back({u, w});
    }

    TreeDiameterDP td(n);
    td.solve();
    std::cout << td.ans << '\n';
    return 0;
}
