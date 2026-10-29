#include <bits/stdc++.h>
using namespace std;
using ll = long long;

// 区间赋值 + 区间求和线段树（懒标记），节点与区间均为 1 下标。
const int maxn = 100000 + 5;  // 按题目规模调整，n 不能超过 maxn

ll base[maxn];       // 原始数组，建树时用 base[1..n]
ll tr[maxn << 2];    // tr[p]：节点 p 管辖区间的和
ll lazy[maxn << 2];  // lazy[p]：待下传的赋值值
bool tag[maxn << 2]; // tag[p]：p 是否挂着未下传的赋值标记

int ls(int p) { return p << 1; }     // p 的左儿子 2p
int rs(int p) { return p << 1 | 1; } // p 的右儿子 2p+1

// 把节点 p 的整个区间 [l, r] 赋值为 value：区间和 = value * 长度
void apply(int p, int l, int r, ll value) {
    tr[p] = value * (r - l + 1);
    lazy[p] = value;
    tag[p] = true;
}

// 下推：把 p 的赋值标记传给两个孩子
void pushdown(int p, int l, int r) {
    // 没有标记无事可做；叶子没有儿子可下放，必须提前返回
    if (!tag[p] || l == r) return;
    int mid = (l + r) >> 1;
    apply(ls(p), l, mid, lazy[p]);
    apply(rs(p), mid + 1, r, lazy[p]);
    tag[p] = false;  // 标记已经下放，清掉
}

// 用 base[1..n] 建树
void seg_build(int p, int l, int r) {
    tag[p] = false;  // 建树时顺手清标记，换数据不必整体清零
    if (l == r) {
        tr[p] = base[l];
        return;
    }
    int mid = (l + r) >> 1;
    seg_build(ls(p), l, mid);
    seg_build(rs(p), mid + 1, r);
    tr[p] = tr[ls(p)] + tr[rs(p)];
}

// 区间赋值：把 [ql, qr] 全部赋值为 value
void assign_range(int ql, int qr, ll value, int p, int l, int r) {
    if (qr < l || r < ql) return;
    if (ql <= l && r <= qr) {
        apply(p, l, r, value);
        return;
    }
    pushdown(p, l, r);  // 要进入孩子，先把标记下放
    int mid = (l + r) >> 1;
    assign_range(ql, qr, value, ls(p), l, mid);
    assign_range(ql, qr, value, rs(p), mid + 1, r);
    tr[p] = tr[ls(p)] + tr[rs(p)];
}

// 区间和：把 [ql, qr] 拆成不交的若干节点，逐个累加
ll range_sum(int ql, int qr, int p, int l, int r) {
    if (qr < l || r < ql) return 0;
    if (ql <= l && r <= qr) return tr[p];
    pushdown(p, l, r);  // 孩子的值可能还没跟上父节点的标记
    int mid = (l + r) >> 1;
    ll answer = range_sum(ql, qr, ls(p), l, mid);
    answer += range_sum(ql, qr, rs(p), mid + 1, r);
    return answer;
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n, m;
    cin >> n >> m;
    for (int i = 1; i <= n; i++) cin >> base[i];
    seg_build(1, 1, n);

    while (m--) {
        int op;
        cin >> op;
        if (op == 1) {
            int l, r;
            ll x;
            cin >> l >> r >> x;
            assign_range(l, r, x, 1, 1, n);
        } else {
            int l, r;
            cin >> l >> r;
            cout << range_sum(l, r, 1, 1, n) << '\n';
        }
    }

    return 0;
}
