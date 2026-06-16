#include <bits/stdc++.h>
using namespace std;

struct SparseTable {
    int n = 0;
    vector<int> lg;
    vector<vector<int>> st;

    void build(const vector<int> &a) {
        n = (int)a.size() - 1; // a 使用 1-indexed。

        lg.assign(n + 1, 0);
        for (int i = 2; i <= n; i++) {
            lg[i] = lg[i / 2] + 1;
        }

        int max_log = lg[n] + 1;
        st.assign(max_log, vector<int>(n + 1, 0));

        for (int i = 1; i <= n; i++) {
            st[0][i] = a[i];
        }

        for (int k = 1; k < max_log; k++) {
            int len = 1 << k;
            int half = len >> 1;
            for (int i = 1; i + len - 1 <= n; i++) {
                st[k][i] = max(st[k - 1][i], st[k - 1][i + half]);
            }
        }
    }

    int query(int l, int r) const {
        int k = lg[r - l + 1];
        return max(st[k][l], st[k][r - (1 << k) + 1]);
    }
};

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n, q;
    cin >> n >> q;

    vector<int> a(n + 1);
    for (int i = 1; i <= n; i++) {
        cin >> a[i];
    }

    SparseTable table;
    table.build(a);

    while (q--) {
        int l, r;
        cin >> l >> r;
        cout << table.query(l, r) << "\n";
    }

    return 0;
}
