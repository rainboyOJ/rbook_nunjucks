#include <algorithm>
#include <vector>

const int maxn = 1e6 + 5;
using ll = long long;
using Edge = struct { int to; ll w; };
using Graph = std::vector<Edge>;
Graph tree[maxn];  // 全局邻接表数组：直接向 tree[u] 加带权边

// 如何使用：
// 1. 读入带权边：tree[u].push_back({v, w}); tree[v].push_back({u, w});
// 2. TreeDiameterDFS td(n);
// 3. td.solve();
//    直径长度在 td.ans，端点是 td.a、td.b，路径是 td.path。

// 两次 DFS 最远点搜索求树的直径，并记录直径路径。
struct TreeDiameterDFS {
    int n;
    int a, b;                // 直径的两个端点
    ll ans;                  // 直径长度
    std::vector<ll> dis;     // dis[u] = 当前 DFS 起点到 u 的距离
    std::vector<int> parent; // parent[u] = 当前 DFS 中 u 的父节点，用于还原路径
    std::vector<int> path;   // 从 a 到 b 的直径路径

    explicit TreeDiameterDFS(int n)
        : n(n), a(0), b(0), ans(0), dis(n + 1), parent(n + 1) {}

    void dfs(int u, int fa, ll d) {
        dis[u] = d;
        parent[u] = fa;
        for (Edge e : tree[u]) {
            int v = e.to;
            if (v == fa) continue;
            dfs(v, u, d + e.w);
        }
    }

    int farthest(int s) {
        dfs(s, 0, 0);
        int far = s;
        for (int u = 1; u <= n; u++) {
            if (dis[u] > dis[far]) far = u;
        }
        return far;
    }

    void build_path() {
        path.clear();
        for (int u = b; u != 0; u = parent[u]) {
            path.push_back(u);
            if (u == a) break;
        }
        std::reverse(path.begin(), path.end());
    }

    // 两次最远点搜索：端点存入 a、b，长度存入 ans，路径存入 path。
    void solve() {
        a = farthest(1);
        b = farthest(a);
        ans = dis[b];
        build_path();
    }
};
