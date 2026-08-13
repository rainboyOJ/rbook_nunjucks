#include <bits/stdc++.h>
using namespace std;

// 区间最小值线段树（单点取 min 更新）
struct MinSegmentTree {
    // 线段树节点：value 为区间最小值
    using T = int;
    static const T INF = 1e9;

    struct Node {
        T value = INF;  // 当前区间的最小值
    };

    // 左儿子 / 右儿子的节点编号
    static int lson(int p) { return p << 1; }
    static int rson(int p) { return p << 1 | 1; }

    // 区间 [l, r] 的中点
    static int mid(int l, int r) { return (l + r) >> 1; }

    int n = 0;              // 区间大小
    vector<Node> tree;      // 线段树数组

    MinSegmentTree(int n = 0) {
        init(n);
    }

    void init(int size) {
        n = size;
        tree.assign(n * 4 + 5, Node{});
    }

    // 上推：用两个孩子的最小值合并出当前节点
    void push_up(int p) {
        tree[p].value = min(tree[lson(p)].value, tree[rson(p)].value);
    }

    // 单点取 min：把位置 pos 的值更新为 min(原值, value)
    void update_min(int pos, T value, int l, int r, int p = 1) {
        if (l == r) {
            tree[p].value = min(tree[p].value, value);
            return;
        }
        int m = mid(l, r);
        if (pos <= m) update_min(pos, value, l, m, lson(p));
        else update_min(pos, value, m + 1, r, rson(p));
        push_up(p);
    }

    // 区间查询：[ql, qr] 的最小值
    T query_min(int ql, int qr, int l, int r, int p = 1) {
        if (ql > qr) return INF;
        if (ql <= l && r <= qr) return tree[p].value;

        int m = mid(l, r);
        T answer = INF;
        if (ql <= m) answer = min(answer, query_min(ql, qr, l, m, lson(p)));
        if (qr > m) answer = min(answer, query_min(ql, qr, m + 1, r, rson(p)));
        return answer;
    }
};

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n, q;
    cin >> n >> q;

    vector<int> a(n + 1);
    vector<int> values;
    values.reserve(n);

    for (int i = 1; i <= n; i++) {
        cin >> a[i];
        values.push_back(a[i]);
    }

    sort(values.begin(), values.end());
    values.erase(unique(values.begin(), values.end()), values.end());

    MinSegmentTree seg((int)values.size());

    for (int i = 1; i <= n; i++) {
        int rank = lower_bound(values.begin(), values.end(), a[i]) - values.begin() + 1;
        seg.update_min(rank, i, 1, seg.n);
    }

    while (q--) {
        int x;
        cin >> x;

        // 所有小于 x 的值，正好落在压缩值域的 [1, last_less]。
        int last_less = lower_bound(values.begin(), values.end(), x) - values.begin();
        int pos = seg.query_min(1, last_less, 1, seg.n);

        if (pos == MinSegmentTree::INF) cout << n + 1 << '\n';
        else cout << pos << '\n';
    }

    return 0;
}
