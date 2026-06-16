#include <bits/stdc++.h>
using namespace std;

template <typename T>
struct SegmentTreeRangeAssign {
    int n = 0;
    vector<T> tree;
    vector<T> lazy;
    vector<bool> has_lazy;

    SegmentTreeRangeAssign(int n = 0) {
        init(n);
    }

    void init(int size) {
        n = size;
        tree.assign(n * 4 + 5, 0);
        lazy.assign(n * 4 + 5, 0);
        has_lazy.assign(n * 4 + 5, false);
    }

    void pull(int p) {
        tree[p] = tree[p << 1] + tree[p << 1 | 1];
    }

    void apply(int p, int l, int r, T value) {
        tree[p] = value * (r - l + 1);
        lazy[p] = value;
        has_lazy[p] = true;
    }

    void push(int p, int l, int r) {
        if (!has_lazy[p] || l == r) return;

        int mid = (l + r) >> 1;
        apply(p << 1, l, mid, lazy[p]);
        apply(p << 1 | 1, mid + 1, r, lazy[p]);
        has_lazy[p] = false;
    }

    void build(const vector<T> &a, int l, int r, int p = 1) {
        if (l == r) {
            tree[p] = a[l];
            return;
        }
        int mid = (l + r) >> 1;
        build(a, l, mid, p << 1);
        build(a, mid + 1, r, p << 1 | 1);
        pull(p);
    }

    void assign_range(int ql, int qr, T value, int l, int r, int p = 1) {
        if (ql <= l && r <= qr) {
            apply(p, l, r, value);
            return;
        }

        push(p, l, r);
        int mid = (l + r) >> 1;
        if (ql <= mid) assign_range(ql, qr, value, l, mid, p << 1);
        if (qr > mid) assign_range(ql, qr, value, mid + 1, r, p << 1 | 1);
        pull(p);
    }

    T query(int ql, int qr, int l, int r, int p = 1) {
        if (ql <= l && r <= qr) return tree[p];

        push(p, l, r);
        int mid = (l + r) >> 1;
        T answer = 0;
        if (ql <= mid) answer += query(ql, qr, l, mid, p << 1);
        if (qr > mid) answer += query(ql, qr, mid + 1, r, p << 1 | 1);
        return answer;
    }
};

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n, m;
    cin >> n >> m;

    vector<long long> a(n + 1);
    for (int i = 1; i <= n; i++) {
        cin >> a[i];
    }

    SegmentTreeRangeAssign<long long> seg(n);
    seg.build(a, 1, n);

    while (m--) {
        int op;
        cin >> op;
        if (op == 1) {
            int l, r;
            long long x;
            cin >> l >> r >> x;
            seg.assign_range(l, r, x, 1, n);
        } else {
            int l, r;
            cin >> l >> r;
            cout << seg.query(l, r, 1, n) << '\n';
        }
    }

    return 0;
}
