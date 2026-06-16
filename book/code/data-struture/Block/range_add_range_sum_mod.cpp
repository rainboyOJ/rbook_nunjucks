#include <bits/stdc++.h>
using namespace std;

using i64 = long long;

struct SqrtDecomposition {
    int n = 0;
    int block_size = 1;
    int block_count = 0;
    vector<i64> a, block_sum, lazy_add;
    vector<int> belong, left_bound, right_bound;

    explicit SqrtDecomposition(const vector<i64>& init) {
        n = (int)init.size() - 1;
        block_size = max(1, (int)sqrt(n));
        block_count = (n + block_size - 1) / block_size;

        a = init;
        block_sum.assign(block_count + 1, 0);
        lazy_add.assign(block_count + 1, 0);
        belong.assign(n + 1, 0);
        left_bound.assign(block_count + 1, 0);
        right_bound.assign(block_count + 1, 0);

        for (int b = 1; b <= block_count; ++b) {
            left_bound[b] = (b - 1) * block_size + 1;
            right_bound[b] = min(n, b * block_size);
            for (int i = left_bound[b]; i <= right_bound[b]; ++i) {
                belong[i] = b;
                block_sum[b] += a[i];
            }
        }
    }

    void add(int l, int r, i64 v) {
        int lb = belong[l];
        int rb = belong[r];

        if (lb == rb) {
            for (int i = l; i <= r; ++i) {
                a[i] += v;
                block_sum[lb] += v;
            }
            return;
        }

        for (int i = l; i <= right_bound[lb]; ++i) {
            a[i] += v;
            block_sum[lb] += v;
        }

        for (int b = lb + 1; b <= rb - 1; ++b) {
            lazy_add[b] += v;
            block_sum[b] += 1LL * (right_bound[b] - left_bound[b] + 1) * v;
        }

        for (int i = left_bound[rb]; i <= r; ++i) {
            a[i] += v;
            block_sum[rb] += v;
        }
    }

    i64 query_mod(int l, int r, i64 mod) const {
        int lb = belong[l];
        int rb = belong[r];
        i64 ans = 0;

        auto add_mod = [&](i64 x) {
            ans = (ans + x) % mod;
        };

        if (lb == rb) {
            for (int i = l; i <= r; ++i) add_mod(a[i] + lazy_add[lb]);
            return ans;
        }

        for (int i = l; i <= right_bound[lb]; ++i) {
            add_mod(a[i] + lazy_add[lb]);
        }

        for (int b = lb + 1; b <= rb - 1; ++b) {
            add_mod(block_sum[b]);
        }

        for (int i = left_bound[rb]; i <= r; ++i) {
            add_mod(a[i] + lazy_add[rb]);
        }

        return ans;
    }
};

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n;
    cin >> n;

    vector<i64> a(n + 1);
    for (int i = 1; i <= n; ++i) cin >> a[i];

    SqrtDecomposition ds(a);

    for (int i = 1; i <= n; ++i) {
        int op, l, r;
        i64 c;
        cin >> op >> l >> r >> c;

        if (op == 0) {
            ds.add(l, r, c);
        } else {
            cout << ds.query_mod(l, r, c + 1) << '\n';
        }
    }

    return 0;
}
