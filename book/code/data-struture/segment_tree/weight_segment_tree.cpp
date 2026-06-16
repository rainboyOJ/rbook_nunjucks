#include <bits/stdc++.h>
using namespace std;

struct WeightSegmentTree {
    struct Node {
        int left = 0;
        int right = 0;
        int sum = 0;
    };

    vector<Node> tree;
    int root = 0;
    int min_value;
    int max_value;

    WeightSegmentTree(int min_value, int max_value)
        : min_value(min_value), max_value(max_value) {
        tree.push_back(Node{});
    }

    int new_node() {
        tree.push_back(Node{});
        return (int)tree.size() - 1;
    }

    int add(int u, int l, int r, int pos, int delta) {
        if (u == 0) u = new_node();
        tree[u].sum += delta;
        if (l == r) return u;

        int mid = l + (r - l) / 2;
        if (pos <= mid) {
            tree[u].left = add(tree[u].left, l, mid, pos, delta);
        } else {
            tree[u].right = add(tree[u].right, mid + 1, r, pos, delta);
        }
        return u;
    }

    void add(int pos, int delta) {
        root = add(root, min_value, max_value, pos, delta);
    }

    int query(int u, int l, int r, int ql, int qr) const {
        if (u == 0 || qr < l || r < ql) return 0;
        if (ql <= l && r <= qr) return tree[u].sum;

        int mid = l + (r - l) / 2;
        return query(tree[u].left, l, mid, ql, qr) +
               query(tree[u].right, mid + 1, r, ql, qr);
    }

    int count_less(int x) const {
        if (x <= min_value) return 0;
        return query(root, min_value, max_value, min_value, x - 1);
    }

    int count_leq(int x) const {
        if (x < min_value) return 0;
        if (x >= max_value) return tree[root].sum;
        return query(root, min_value, max_value, min_value, x);
    }

    int kth(int u, int l, int r, int k) const {
        if (l == r) return l;

        int left_sum = tree[u].left ? tree[tree[u].left].sum : 0;
        int mid = l + (r - l) / 2;
        if (k <= left_sum) return kth(tree[u].left, l, mid, k);
        return kth(tree[u].right, mid + 1, r, k - left_sum);
    }

    int kth(int k) const {
        return kth(root, min_value, max_value, k);
    }

    int predecessor(int x) const {
        int cnt = count_less(x);
        return kth(cnt);
    }

    int successor(int x) const {
        int cnt = count_leq(x);
        return kth(cnt + 1);
    }

    int size() const {
        return root == 0 ? 0 : tree[root].sum;
    }
};

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n;
    cin >> n;

    // Value range for Luogu P3369 ordinary balance tree template.
    WeightSegmentTree seg(-10000000, 10000000);

    while (n--) {
        int op, x;
        cin >> op >> x;

        if (op == 1) {
            seg.add(x, 1);
        } else if (op == 2) {
            seg.add(x, -1);
        } else if (op == 3) {
            cout << seg.count_less(x) + 1 << '\n';
        } else if (op == 4) {
            cout << seg.kth(x) << '\n';
        } else if (op == 5) {
            cout << seg.predecessor(x) << '\n';
        } else if (op == 6) {
            cout << seg.successor(x) << '\n';
        }
    }

    return 0;
}
