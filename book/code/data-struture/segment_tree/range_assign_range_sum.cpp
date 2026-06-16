#include <bits/stdc++.h>
using namespace std;

// Segment tree: range assign, range sum query.
// Index range is [1, n].

const int maxn = 100000 + 5;

template <typename T, int N = maxn>
struct SegmentTreeRangeAssign {
    T tr[N * 4 + 5]{};
    T lazy[N * 4 + 5]{};
    bool has_lazy[N * 4 + 5]{};

    int left_child(int p) const { return p << 1; }
    int right_child(int p) const { return p << 1 | 1; }
    int mid(int l, int r) const { return (l + r) >> 1; }

    void apply(int p, int l, int r, T value) {
        tr[p] = value * (r - l + 1);
        lazy[p] = value;
        has_lazy[p] = true;
    }

    void push_up(int p) {
        tr[p] = tr[left_child(p)] + tr[right_child(p)];
    }

    void push_down(int p, int l, int r) {
        if (!has_lazy[p] || l == r) return;
        int m = mid(l, r);
        apply(left_child(p), l, m, lazy[p]);
        apply(right_child(p), m + 1, r, lazy[p]);
        has_lazy[p] = false;
    }

    void build(int l, int r, int p = 1) {
        if (l == r) {
            cin >> tr[p];
            return;
        }
        int m = mid(l, r);
        build(l, m, left_child(p));
        build(m + 1, r, right_child(p));
        push_up(p);
    }

    void assign_range(int ql, int qr, T value, int l, int r, int p = 1) {
        if (ql <= l && r <= qr) {
            apply(p, l, r, value);
            return;
        }
        push_down(p, l, r);

        int m = mid(l, r);
        if (ql <= m) assign_range(ql, qr, value, l, m, left_child(p));
        if (qr > m) assign_range(ql, qr, value, m + 1, r, right_child(p));
        push_up(p);
    }

    T query(int ql, int qr, int l, int r, int p = 1) {
        if (ql <= l && r <= qr) return tr[p];
        push_down(p, l, r);

        int m = mid(l, r);
        T ans = 0;
        if (ql <= m) ans += query(ql, qr, l, m, left_child(p));
        if (qr > m) ans += query(ql, qr, m + 1, r, right_child(p));
        return ans;
    }
};

SegmentTreeRangeAssign<long long> seg;
