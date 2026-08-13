#include <bits/stdc++.h>
using namespace std;

struct SegmentTreeRangeAssign {
    struct Node {
        long long value = 0;
        long long lazy = 0;
        bool has_lazy = false;

        Node operator+(const Node &other) const {
            return Node{value + other.value, 0, false};
        }
    };

    static const int lson(int p) { return p << 1; }
    static const int rson(int p) { return p << 1 | 1; }

    int n = 0;
    vector<Node> tree;

    SegmentTreeRangeAssign(int n = 0) {
        init(n);
    }

    void init(int size) {
        n = size;
        tree.assign(n * 4 + 5, Node{});
    }

    void pull(int p) {
        tree[p] = tree[p << 1] + tree[p << 1 | 1];
    }

    void apply(int p, int l, int r, long long value) {
        tree[p].value = value * (r - l + 1);
        tree[p].lazy = value;
        tree[p].has_lazy = true;
    }

    void push(int p, int l, int r) {
        if (!tree[p].has_lazy || l == r) return;

        int mid = (l + r) >> 1;
        apply(p << 1, l, mid, tree[p].lazy);
        apply(p << 1 | 1, mid + 1, r, tree[p].lazy);
        tree[p].has_lazy = false;
    }

    void build(const vector<long long> &a, int l, int r, int p = 1) {
        if (l == r) {
            tree[p].value = a[l];
            return;
        }
        int mid = (l + r) >> 1;
        build(a, l, mid, p << 1);
        build(a, mid + 1, r, p << 1 | 1);
        pull(p);
    }

    void assign_range(int ql, int qr, long long value, int l, int r, int p = 1) {
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

    long long query(int ql, int qr, int l, int r, int p = 1) {
        if (ql <= l && r <= qr) return tree[p].value;

        push(p, l, r);
        int mid = (l + r) >> 1;
        long long answer = 0;
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

    SegmentTreeRangeAssign seg(n);
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
