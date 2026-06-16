#include <bits/stdc++.h>
using namespace std;

using ll = long long;

struct DifferenceArray {
    int n = 0;
    vector<ll> diff;

    DifferenceArray() = default;

    explicit DifferenceArray(const vector<ll>& a) {
        init(a);
    }

    // a 使用 0 下标存储；diff 使用 1 下标，方便处理区间 [l, r]。
    void init(const vector<ll>& a) {
        n = (int)a.size();
        diff.assign(n + 2, 0);
        for (int i = 1; i <= n; i++) {
            diff[i] = a[i - 1] - (i == 1 ? 0 : a[i - 2]);
        }
    }

    // 对原数组的 1 下标闭区间 [l, r] 全部加 v。
    void add(int l, int r, ll v) {
        diff[l] += v;
        diff[r + 1] -= v;
    }

    // 将差分数组还原成 1 下标原数组。
    vector<ll> restore() const {
        vector<ll> a(n + 1, 0);
        for (int i = 1; i <= n; i++) {
            a[i] = a[i - 1] + diff[i];
        }
        return a;
    }
};

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n, m;
    cin >> n >> m;

    vector<ll> a(n);
    for (int i = 0; i < n; i++) {
        cin >> a[i];
    }

    DifferenceArray da(a);

    while (m--) {
        int l, r;
        ll v;
        cin >> l >> r >> v;
        da.add(l, r, v);
    }

    vector<ll> ans = da.restore();
    for (int i = 1; i <= n; i++) {
        if (i > 1) cout << ' ';
        cout << ans[i];
    }
    cout << '\n';

    return 0;
}
