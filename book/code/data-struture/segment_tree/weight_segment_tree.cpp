#include <bits/stdc++.h>
using namespace std;

// 权值线段树（动态开点）：按值域统计数的个数，支持 kth / 前驱 / 后继
struct WeightSegmentTree {
    // 线段树节点：left/right 为孩子下标，sum 为值域区间内的元素个数
    using T = int;
    struct Node {
        int left = 0;   // 左孩子下标，0 表示空
        int right = 0;  // 右孩子下标，0 表示空
        T sum = 0;      // 值域区间内的元素个数
    };

    vector<Node> tree;  // 动态节点池，节点下标即指针
    int root = 0;       // 根节点下标
    int min_value;      // 值域下界
    int max_value;      // 值域上界

    WeightSegmentTree(int min_value, int max_value)
        : min_value(min_value), max_value(max_value) {
        tree.push_back(Node{});  // 0 号节点为空节点
    }

    // 区间 [l, r] 的中点
    static int mid(int l, int r) { return (l + r) >> 1; }

    // 新建一个空节点
    int new_node() {
        tree.push_back(Node{});
        return (int)tree.size() - 1;
    }

    // 在值域区间 [l, r] 的节点 u 上插入位置 pos，个数增加 delta
    int add(int u, int l, int r, int pos, int delta) {
        if (u == 0) u = new_node();
        tree[u].sum += delta;
        if (l == r) return u;

        int m = mid(l, r);
        if (pos <= m) {
            tree[u].left = add(tree[u].left, l, m, pos, delta);
        } else {
            tree[u].right = add(tree[u].right, m + 1, r, pos, delta);
        }
        return u;
    }

    // 插入位置 pos，个数增加 delta
    void add(int pos, int delta) {
        root = add(root, min_value, max_value, pos, delta);
    }

    // 查询值域区间 [ql, qr] 的元素个数
    int query(int u, int l, int r, int ql, int qr) const {
        if (u == 0 || qr < l || r < ql) return 0;
        if (ql <= l && r <= qr) return tree[u].sum;

        int m = mid(l, r);
        return query(tree[u].left, l, m, ql, qr) +
               query(tree[u].right, m + 1, r, ql, qr);
    }

    // 小于 x 的元素个数
    int count_less(int x) const {
        if (x <= min_value) return 0;
        return query(root, min_value, max_value, min_value, x - 1);
    }

    // 小于等于 x 的元素个数
    int count_leq(int x) const {
        if (x < min_value) return 0;
        if (x >= max_value) return tree[root].sum;
        return query(root, min_value, max_value, min_value, x);
    }

    // 第 k 小（k 从 1 开始）
    int kth(int u, int l, int r, int k) const {
        if (l == r) return l;

        int left_sum = tree[u].left ? tree[tree[u].left].sum : 0;
        int m = mid(l, r);
        if (k <= left_sum) return kth(tree[u].left, l, m, k);
        return kth(tree[u].right, m + 1, r, k - left_sum);
    }

    int kth(int k) const {
        return kth(root, min_value, max_value, k);
    }

    // 小于 x 的最大元素
    int predecessor(int x) const {
        int cnt = count_less(x);
        return kth(cnt);
    }

    // 大于 x 的最小元素
    int successor(int x) const {
        int cnt = count_leq(x);
        return kth(cnt + 1);
    }

    // 元素总个数
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
