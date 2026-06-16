#include <bits/stdc++.h>
using namespace std;

template <typename T>
struct SegmentTreePointAdd {
    int n = 0;
    vector<T> tree;

    SegmentTreePointAdd(int n = 0) {
        init(n);
    }

    void init(int size) {
        n = size;
        tree.assign(n * 4 + 5, 0);
    }

    void pull(int p) {
        tree[p] = tree[p << 1] + tree[p << 1 | 1];
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

    void add(int pos, T value, int l, int r, int p = 1) {
        if (l == r) {
            tree[p] += value;
            return;
        }
        int mid = (l + r) >> 1;
        if (pos <= mid) add(pos, value, l, mid, p << 1);
        else add(pos, value, mid + 1, r, p << 1 | 1);
        pull(p);
    }

    T query(int ql, int qr, int l, int r, int p = 1) const {
        if (ql <= l && r <= qr) return tree[p];

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

    SegmentTreePointAdd<long long> seg(n);
    seg.build(a, 1, n);

    while (m--) {
        int op;
        cin >> op;
        if (op == 1) {
            int x;
            long long k;
            cin >> x >> k;
            seg.add(x, k, 1, n);
        } else {
            int l, r;
            cin >> l >> r;
            cout << seg.query(l, r, 1, n) << '\n';
        }
    }

    return 0;
}
