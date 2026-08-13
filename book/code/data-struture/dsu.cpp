#include <bits/stdc++.h>
using namespace std;

// 并查集：合并两个集合，查询两个元素是否在同一集合
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

    // 合并 x 与 y 所在集合（按大小合并），返回是否发生合并
    bool unite(int x, int y) {
        int fx = find(x);
        int fy = find(y);
        if (fx == fy) return false;

        if (sz[fx] < sz[fy]) swap(fx, fy);
        fa[fy] = fx;
        sz[fx] += sz[fy];
        return true;
    }
};

int main() {
    int n, m;
    cin >> n >> m;

    DSU dsu(n);
    while (m--) {
        int op, x, y;
        cin >> op >> x >> y;
        if (op == 1) {
            dsu.unite(x, y);
        } else if (op == 2) {
            cout << (dsu.same(x, y) ? "Y" : "N") << "\n";
        }
    }

    return 0;
}
