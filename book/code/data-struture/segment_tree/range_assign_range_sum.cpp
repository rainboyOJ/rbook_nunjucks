#include <bits/stdc++.h>
using namespace std;

// 区间赋值 + 区间求和线段树（懒标记）
struct SegmentTreeRangeAssign {
    // 线段树节点：value 为区间和，lazy 为待下传的赋值标记
    struct Node {
        long long value = 0;    // 当前区间的真实区间和
        long long lazy = 0;     // 待下传的赋值值
        bool has_lazy = false;  // 是否还有未下传的赋值标记

        // 合并两个孩子：区间和相加，合并结果不携带懒标记
        Node operator+(const Node &other) const {
            return Node{value + other.value, 0, false};
        }
    };

    // 左儿子 / 右儿子的节点编号
    static int lson(int p) { return p << 1; }
    static int rson(int p) { return p << 1 | 1; }

    int n = 0;              // 区间大小
    vector<Node> tree;      // 线段树数组

    SegmentTreeRangeAssign(int n = 0) {
        init(n);
    }

    void init(int size) {
        n = size;
        tree.assign(n * 4 + 5, Node{});
    }

    // 上推：用两个孩子合并出当前节点
    void push_up(int p) {
        tree[p] = tree[lson(p)] + tree[rson(p)];
    }

    // 把节点 p 的整个区间 [l, r] 赋值为 value
    void apply(int p, int l, int r, long long value) {
        tree[p].value = value * (r - l + 1);
        tree[p].lazy = value;
        tree[p].has_lazy = true;
    }

    // 下推：把节点 p 的懒标记传给两个孩子
    void push_down(int p, int l, int r) {
        if (!tree[p].has_lazy || l == r) return;

        int mid = (l + r) >> 1;
        apply(lson(p), l, mid, tree[p].lazy);
        apply(rson(p), mid + 1, r, tree[p].lazy);
        tree[p].has_lazy = false;
    }

    // 用数组 a 建树
    void build(const vector<long long> &a, int l, int r, int p = 1) {
        if (l == r) {
            tree[p].value = a[l];
            return;
        }
        int mid = (l + r) >> 1;
        build(a, l, mid, lson(p));
        build(a, mid + 1, r, rson(p));
        push_up(p);
    }

    // 区间赋值：把 [ql, qr] 全部赋值为 value
    void assign_range(int ql, int qr, long long value, int l, int r, int p = 1) {
        if (ql <= l && r <= qr) {
            apply(p, l, r, value);
            return;
        }

        push_down(p, l, r);
        int mid = (l + r) >> 1;
        if (ql <= mid) assign_range(ql, qr, value, l, mid, lson(p));
        if (qr > mid) assign_range(ql, qr, value, mid + 1, r, rson(p));
        push_up(p);
    }

    // 区间查询：[ql, qr] 的区间和
    long long query(int ql, int qr, int l, int r, int p = 1) {
        if (ql <= l && r <= qr) return tree[p].value;

        push_down(p, l, r);
        int mid = (l + r) >> 1;
        long long answer = 0;
        if (ql <= mid) answer += query(ql, qr, l, mid, lson(p));
        if (qr > mid) answer += query(ql, qr, mid + 1, r, rson(p));
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
