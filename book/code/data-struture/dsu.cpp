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
