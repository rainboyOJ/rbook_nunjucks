#include <bits/stdc++.h>
using namespace std;

using ll = long long;

struct Difference2D {
    int n = 0, m = 0;
    vector<vector<ll>> diff;

    Difference2D() = default;

    Difference2D(int n_, int m_) {
        init(n_, m_);
    }

    void init(int n_, int m_) {
        n = n_;
        m = m_;
        diff.assign(n + 2, vector<ll>(m + 2, 0));
    }

    // 对原矩阵的子矩形 [(x1, y1), (x2, y2)] 全部加 v，坐标从 1 开始。
    void add(int x1, int y1, int x2, int y2, ll v) {
        diff[x1][y1] += v;
        diff[x2 + 1][y1] -= v;
        diff[x1][y2 + 1] -= v;
        diff[x2 + 1][y2 + 1] += v;
    }

    vector<vector<ll>> restore() const {
        vector<vector<ll>> a(n + 1, vector<ll>(m + 1, 0));
        for (int i = 1; i <= n; i++) {
            for (int j = 1; j <= m; j++) {
                a[i][j] = a[i - 1][j] + a[i][j - 1] - a[i - 1][j - 1] + diff[i][j];
            }
        }
        return a;
    }
};

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n, m, q;
    cin >> n >> m >> q;

    Difference2D d(n, m);

    for (int i = 1; i <= n; i++) {
        for (int j = 1; j <= m; j++) {
            ll x;
            cin >> x;
            d.add(i, j, i, j, x);
        }
    }

    while (q--) {
        int x1, y1, x2, y2;
        ll v;
        cin >> x1 >> y1 >> x2 >> y2 >> v;
        d.add(x1, y1, x2, y2, v);
    }

    vector<vector<ll>> ans = d.restore();
    for (int i = 1; i <= n; i++) {
        for (int j = 1; j <= m; j++) {
            if (j > 1) cout << ' ';
            cout << ans[i][j];
        }
        cout << '\n';
    }

    return 0;
}
