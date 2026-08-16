#include <bits/stdc++.h>
using namespace std;

using Graph = vector<vector<int>>;
Graph tree;  // 全局邻接表：使用前先 resize(n+1) 并加边

// 求树的所有重心。
// 重心：删除该点后，剩下的每个连通块大小都不超过 n/2。
struct TreeCentroid {
    int n;
    vector<int> sz;   // sz[u] = u 的子树大小
    vector<int> ans;  // 答案：所有重心，按编号升序
    int best;         // 最小的 B(u)：删除 u 后最大的连通块大小

    explicit TreeCentroid(int n) : n(n), best(n), sz(n + 1) {}

    // 返回所有重心（编号升序）
    vector<int> find_centroids(int root = 1) {
        best = n;
        ans.clear();
        dfs(root, 0);
        sort(ans.begin(), ans.end());
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
            mx = max(mx, sz[v]);
        }

        // 父亲方向也是一块：整棵树减去 u 的子树
        mx = max(mx, n - sz[u]);

        // 记录 B(u) 最小的点（可能不止一个）
        if (mx < best) {
            best = mx;
            ans = {u};
        } else if (mx == best) {
            ans.push_back(u);
        }
    }
};

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n;
    cin >> n;

    tree.resize(n + 1);
    for (int i = 1; i < n; ++i) {
        int u, v;
        cin >> u >> v;
        tree[u].push_back(v);
        tree[v].push_back(u);
    }

    TreeCentroid tc(n);
    vector<int> answer = tc.find_centroids();
    cout << answer.size() << '\n';
    for (size_t i = 0; i < answer.size(); ++i) {
        if (i > 0) cout << ' ';
        cout << answer[i];
    }
    cout << '\n';
    return 0;
}
