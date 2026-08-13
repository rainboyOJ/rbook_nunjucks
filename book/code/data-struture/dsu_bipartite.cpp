#include <bits/stdc++.h>
using namespace std;

// 并查集判二分图：把每个点拆成"本体"和"对立面"，矛盾即非二分图
struct DSU {
    using T = int;

    vector<T> fa;  // fa[x] 为 x 的父节点
    vector<T> sz;  // sz[x] 为集合大小（仅根节点有效）

    DSU(int n = 0) {
        init(n);
    }

    void init(int n) {
        fa.resize(n + 1);
        sz.assign(n + 1, 1);
        iota(fa.begin(), fa.end(), 0);
    }

    // 查询 x 所在集合的根（路径压缩）
    int find(int x) {
        if (fa[x] == x) return x;
        return fa[x] = find(fa[x]);
    }

    // x 与 y 是否在同一集合
    bool same(int x, int y) {
        return find(x) == find(y);
    }

    // 合并 x 与 y 所在集合（按大小合并）
    void unite(int x, int y) {
        int fx = find(x);
        int fy = find(y);
        if (fx == fy) return;

        if (sz[fx] < sz[fy]) swap(fx, fy);
        fa[fy] = fx;
        sz[fx] += sz[fy];
    }
};

int main() {
    int n, m;
    cin >> n >> m;

    // 拆点：i 表示点 i，i+n 表示"点 i 的对立面"
    DSU dsu(2 * n);
    auto enemy = [n](int x) {
        return x + n;
    };

    bool ok = true;
    while (m--) {
        int x, y;
        cin >> x >> y;

        // x 与 y 必须在不同类别中。
        if (dsu.same(x, y)) {
            ok = false;
        }
        dsu.unite(x, enemy(y));
        dsu.unite(enemy(x), y);
    }

    cout << (ok ? "YES" : "NO") << "\n";
    return 0;
}
