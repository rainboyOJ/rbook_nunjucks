#include <bits/stdc++.h>
using namespace std;

const int maxn = 1e6 + 5;
using ll = long long;
using Edge = struct { int to; ll w; };
using Graph = vector<Edge>;
Graph tree[maxn];  // 全局邻接表数组：直接向 tree[u] 加带权边

// 求树的直径：任意点出发找最远点 u，再从 u 找最远点 v，u -> v 就是直径。
struct TreeDiameter {
    int n;
    int a, b;            // 直径的两个端点
    ll ans;              // 直径长度
    vector<ll> dis;      // 到当前起点的距离

    explicit TreeDiameter(int n) : n(n), a(0), b(0), ans(0), dis(n + 1) {}

    // 从 s 出发找最远点，返回最远点的编号
    int farthest(int s) {
        dis.assign(n + 1, -1);
        queue<int> q;
        q.push(s);
        dis[s] = 0;
        int far = s;

        while (!q.empty()) {
            int u = q.front();
            q.pop();
            if (dis[u] > dis[far]) far = u;
            for (Edge e : tree[u]) {
                int v = e.to;
                ll w = e.w;
                if (dis[v] != -1) continue;
                dis[v] = dis[u] + w;
                q.push(v);
            }
        }
        return far;
    }

    // 两次最远点搜索：端点存入 a、b，长度存入 ans
    void solve() {
        a = farthest(1);
        b = farthest(a);
        ans = dis[b];
    }
};

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n;
    cin >> n;

    for (int i = 1; i < n; ++i) {
        int u, v;
        ll w;
        cin >> u >> v >> w;
        tree[u].push_back({v, w});
        tree[v].push_back({u, w});
    }

    TreeDiameter td(n);
    td.solve();
    cout << td.ans << '\n';
    return 0;
}
