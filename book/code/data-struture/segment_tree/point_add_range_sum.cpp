#include <bits/stdc++.h>
using namespace std;

// Segment tree: point add, range sum query.
// Index range is [1, n].

const int maxn = 100000 + 5;

template <typename T, int N = maxn>
struct SegmentTreePointAdd {
    T tr[N * 4 + 5]{};

    int left_child(int p) const { return p << 1; }
    int right_child(int p) const { return p << 1 | 1; }
    int mid(int l, int r) const { return (l + r) >> 1; }

    void push_up(int p) {
        tr[p] = tr[left_child(p)] + tr[right_child(p)];
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

    void add(int pos, T value, int l, int r, int p = 1) {
        if (l == r) {
            tr[p] += value;
            return;
        }
        int m = mid(l, r);
        if (pos <= m) add(pos, value, l, m, left_child(p));
        else add(pos, value, m + 1, r, right_child(p));
        push_up(p);
    }

    T query(int ql, int qr, int l, int r, int p = 1) {
        if (ql <= l && r <= qr) return tr[p];

        int m = mid(l, r);
        T ans = 0;
        if (ql <= m) ans += query(ql, qr, l, m, left_child(p));
        if (qr > m) ans += query(ql, qr, m + 1, r, right_child(p));
        return ans;
    }
};

SegmentTreePointAdd<long long> seg;
