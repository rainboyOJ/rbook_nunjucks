#include <bits/stdc++.h>
using namespace std;

// 可持久化数组：每次修改产生一个新版本，旧版本仍然可查询
struct PersistentArray {
    // 线段树节点：left/right 为左右孩子下标，value 为单点值
    using T = long long;
    struct Node {
        int left = 0;   // 左孩子下标，0 表示空
        int right = 0;  // 右孩子下标，0 表示空
        T value = 0;    // 叶子节点存数组值
    };

    vector<Node> tree;  // 动态节点池，节点下标即指针

    explicit PersistentArray(int max_nodes) {
        tree.reserve(max_nodes);
        tree.push_back(Node{});  // 0 号节点为空节点
    }

    // 区间 [l, r] 的中点
    static int mid(int l, int r) { return (l + r) >> 1; }

    // 复制节点 p 并返回新节点下标
    int clone(int p) {
        tree.push_back(tree[p]);
        return (int)tree.size() - 1;
    }

    // 用数组 a 建树，返回根节点下标
    int build(int l, int r, const vector<T> &a) {
        int p = clone(0);
        if (l == r) {
            tree[p].value = a[l];
            return p;
        }
        int m = mid(l, r);
        tree[p].left = build(l, m, a);
        tree[p].right = build(m + 1, r, a);
        return p;
    }

    // 基于版本 p 修改位置 pos 为 value，产生新版本，返回新根下标
    int update(int p, int l, int r, int pos, T value) {
        int q = clone(p);
        if (l == r) {
            tree[q].value = value;
            return q;
        }
        int m = mid(l, r);
        if (pos <= m) {
            tree[q].left = update(tree[p].left, l, m, pos, value);
        } else {
            tree[q].right = update(tree[p].right, m + 1, r, pos, value);
        }
        return q;
    }

    // 查询版本 p 中位置 pos 的值
    T query(int p, int l, int r, int pos) const {
        if (l == r) return tree[p].value;
        int m = mid(l, r);
        if (pos <= m) return query(tree[p].left, l, m, pos);
        return query(tree[p].right, m + 1, r, pos);
    }
};

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n, m;
    cin >> n >> m;

    vector<long long> a(n + 1);
    for (int i = 1; i <= n; ++i) cin >> a[i];

    int max_nodes = n + m * 20 + 5;
    PersistentArray seg(max_nodes);

    vector<int> root(m + 1);
    root[0] = seg.build(1, n, a);

    for (int i = 1; i <= m; ++i) {
        int version, op, pos;
        cin >> version >> op >> pos;
        if (op == 1) {
            long long value;
            cin >> value;
            root[i] = seg.update(root[version], 1, n, pos, value);
        } else {
            root[i] = root[version];
            cout << seg.query(root[version], 1, n, pos) << '\n';
        }
    }

    return 0;
}
