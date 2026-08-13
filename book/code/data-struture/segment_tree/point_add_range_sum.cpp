#include <bits/stdc++.h>
using namespace std;

// 单点加 + 区间求和线段树
struct SegmentTreePointAdd {
    // 线段树节点：value 为区间和
    using T = long long;
    struct Node {
        T value = 0;    // 当前区间的区间和

        // 合并两个孩子：区间和相加
        Node operator+(const Node &other) const {
            return Node{value + other.value};
        }
    };

    // 左儿子 / 右儿子的节点编号
    static int lson(int p) { return p << 1; }
    static int rson(int p) { return p << 1 | 1; }

    // 区间 [l, r] 的中点
    static int mid(int l, int r) { return (l + r) >> 1; }

    int n = 0;              // 区间大小
    vector<Node> tree;      // 线段树数组

    SegmentTreePointAdd(int n = 0) {
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

    // 用数组 a 建树
    void build(const vector<T> &a, int l, int r, int p = 1) {
        if (l == r) {
            tree[p].value = a[l];
            return;
        }
        int m = mid(l, r);
        build(a, l, m, lson(p));
        build(a, m + 1, r, rson(p));
        push_up(p);
    }

    // 单点加：给位置 pos 增加 value
    void add(int pos, T value, int l, int r, int p = 1) {
        if (l == r) {
            tree[p].value += value;
            return;
        }
        int m = mid(l, r);
        if (pos <= m) add(pos, value, l, m, lson(p));
        else add(pos, value, m + 1, r, rson(p));
        push_up(p);
    }

    // 区间查询：[ql, qr] 的区间和
    T query(int ql, int qr, int l, int r, int p = 1) {
        if (ql <= l && r <= qr) return tree[p].value;

        int m = mid(l, r);
        T answer = 0;
        if (ql <= m) answer += query(ql, qr, l, m, lson(p));
        if (qr > m) answer += query(ql, qr, m + 1, r, rson(p));
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

    SegmentTreePointAdd seg(n);
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
