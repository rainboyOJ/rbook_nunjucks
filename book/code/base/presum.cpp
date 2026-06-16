#include <bits/stdc++.h>
using namespace std;

using ll = long long;

struct PrefixSum {
    vector<ll> s;

    PrefixSum() = default;

    explicit PrefixSum(const vector<ll>& a) {
        init(a);
    }

    // a 使用 0 下标存储，s[i] 表示前 i 个数的和。
    void init(const vector<ll>& a) {
        int n = (int)a.size();
        s.assign(n + 1, 0);
        for (int i = 1; i <= n; i++) {
            s[i] = s[i - 1] + a[i - 1];
        }
    }

    // 查询 1 下标闭区间 [l, r] 的区间和。
    ll query(int l, int r) const {
        return s[r] - s[l - 1];
    }
};

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n, q;
    cin >> n >> q;

    vector<ll> a(n);
    for (int i = 0; i < n; i++) {
        cin >> a[i];
    }

    PrefixSum ps(a);

    while (q--) {
        int l, r;
        cin >> l >> r;
        cout << ps.query(l, r) << '\n';
    }

    return 0;
}
