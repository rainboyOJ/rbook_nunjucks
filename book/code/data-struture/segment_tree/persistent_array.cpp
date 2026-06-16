#include <bits/stdc++.h>
using namespace std;

struct PersistentArray {
    struct Node {
        int left = 0;
        int right = 0;
        int value = 0;
    };

    vector<Node> tree;

    explicit PersistentArray(int max_nodes) {
        tree.reserve(max_nodes);
        tree.push_back(Node{});
    }

    int clone(int p) {
        tree.push_back(tree[p]);
        return (int)tree.size() - 1;
    }

    int build(int l, int r, const vector<int>& a) {
        int p = clone(0);
        if (l == r) {
            tree[p].value = a[l];
            return p;
        }
        int mid = (l + r) >> 1;
        tree[p].left = build(l, mid, a);
        tree[p].right = build(mid + 1, r, a);
        return p;
    }

    int update(int p, int l, int r, int pos, int value) {
        int q = clone(p);
        if (l == r) {
            tree[q].value = value;
            return q;
        }
        int mid = (l + r) >> 1;
        if (pos <= mid) {
            tree[q].left = update(tree[p].left, l, mid, pos, value);
        } else {
            tree[q].right = update(tree[p].right, mid + 1, r, pos, value);
        }
        return q;
    }

    int query(int p, int l, int r, int pos) const {
        if (l == r) return tree[p].value;
        int mid = (l + r) >> 1;
        if (pos <= mid) return query(tree[p].left, l, mid, pos);
        return query(tree[p].right, mid + 1, r, pos);
    }
};

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n, m;
    cin >> n >> m;

    vector<int> a(n + 1);
    for (int i = 1; i <= n; ++i) cin >> a[i];

    int max_nodes = n + m * 20 + 5;
    PersistentArray seg(max_nodes);

    vector<int> root(m + 1);
    root[0] = seg.build(1, n, a);

    for (int i = 1; i <= m; ++i) {
        int version, op, pos;
        cin >> version >> op >> pos;
        if (op == 1) {
            int value;
            cin >> value;
            root[i] = seg.update(root[version], 1, n, pos, value);
        } else {
            root[i] = root[version];
            cout << seg.query(root[version], 1, n, pos) << '\n';
        }
    }

    return 0;
}
