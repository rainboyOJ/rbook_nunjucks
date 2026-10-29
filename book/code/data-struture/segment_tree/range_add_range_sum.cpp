#include <bits/stdc++.h>
using namespace std;
using ll = long long;

// 懒标记线段树（区间加 + 区间和，取模），节点与区间均为 1 下标。
// 这段 sgt 与 HLD 模板里的 sgt 段完全一致，可以整段搬到树链剖分里用。
const int maxn = 100000 + 5;  // 按题目规模调整，n 不能超过 maxn

int mod = 1000000007;  // 取模的模数，做题时按题面赋值

ll base[maxn];       // 原始数组，建树时用 base[1..n]
ll tr[maxn << 2];    // tr[p]：节点 p 管辖区间的和
ll lazy[maxn << 2];  // lazy[p]：待下传给儿子的加法标记

// 把可能为负的 x 归一到 [0, mod)：只用在“外来值”上（输入的初值和增量）。
ll norm(ll x) { return (x % mod + mod) % mod; }

int ls(int p) { return p << 1; }     // p 的左儿子 2p
int rs(int p) { return p << 1 | 1; } // p 的右儿子 2p+1

// 把节点 p 的整个区间 [l, r] 整体加 value：区间和加上 value * 长度
void apply(int p, int l, int r, ll value) {
    value = norm(value);
    tr[p] = (tr[p] + value * (r - l + 1)) % mod;
    lazy[p] = (lazy[p] + value) % mod;
}

void pushdown(int p, int l, int r) {
    // 懒标记为 0 无事可做；叶子没有儿子可下放，必须提前返回，
    // 否则 mid = l，会递归进入自己并且写越界的 tr[2p]。
    if (!lazy[p] || l == r) return;
    int mid = (l + r) >> 1;
    apply(ls(p), l, mid, lazy[p]);
    apply(rs(p), mid + 1, r, lazy[p]);
    lazy[p] = 0;
}

// 用 base[1..n] 建树
void seg_build(int p, int l, int r) {
    lazy[p] = 0;  // 建树时顺手清懒标记，换数据不必整体清零
    if (l == r) {
        tr[p] = norm(base[l]);
        return;
    }
    int mid = (l + r) >> 1;
    seg_build(ls(p), l, mid);
    seg_build(rs(p), mid + 1, r);
    tr[p] = (tr[ls(p)] + tr[rs(p)]) % mod;
}

// 区间加：把 [ql, qr] 整体加 value。
void range_add(int ql, int qr, ll value, int p, int l, int r) {
    if (qr < l || r < ql) return;
    if (ql <= l && r <= qr) {
        apply(p, l, r, value);
        return;
    }
    pushdown(p, l, r);
    int mid = (l + r) >> 1;
    range_add(ql, qr, value, ls(p), l, mid);
    range_add(ql, qr, value, rs(p), mid + 1, r);
    tr[p] = (tr[ls(p)] + tr[rs(p)]) % mod;
}

// 区间和：把 [ql, qr] 拆成不交的若干节点，逐个累加。
ll range_sum(int ql, int qr, int p, int l, int r) {
    if (qr < l || r < ql) return 0;
    if (ql <= l && r <= qr) return tr[p];
    pushdown(p, l, r);
    int mid = (l + r) >> 1;
    ll answer = range_sum(ql, qr, ls(p), l, mid);
    answer += range_sum(ql, qr, rs(p), mid + 1, r);
    return answer % mod;
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
            range_add(l, r, x, 1, 1, n);
        } else {
            int l, r;
            cin >> l >> r;
            cout << range_sum(l, r, 1, 1, n) << '\n';
        }
    }

    return 0;
}
