#include <bits/stdc++.h>
using namespace std;

struct DSU {
    vector<int> fa, sz;

    DSU(int n = 0) {
        init(n);
    }

    void init(int n) {
        fa.resize(n + 1);
        sz.assign(n + 1, 1);
        iota(fa.begin(), fa.end(), 0);
    }

    int find(int x) {
        if (fa[x] == x) return x;
        return fa[x] = find(fa[x]);
    }

    bool same(int x, int y) {
        return find(x) == find(y);
    }

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
