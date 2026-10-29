#include <bits/stdc++.h>
using namespace std;
using ll = long long;

// 单点加 + 区间求和线段树，节点与区间均为 1 下标。
const int maxn = 100000 + 5;  // 按题目规模调整，n 不能超过 maxn

ll base[maxn];     // 原始数组，建树时用 base[1..n]
ll tr[maxn << 2];  // tr[p]：节点 p 管辖区间的和

int ls(int p) { return p << 1; }     // p 的左儿子 2p
int rs(int p) { return p << 1 | 1; } // p 的右儿子 2p+1

// 上推：父区间的和 = 两个儿子的和
void pushup(int p) { tr[p] = tr[ls(p)] + tr[rs(p)]; }

// 用 base[1..n] 建树
void seg_build(int p, int l, int r) {
    if (l == r) {
        tr[p] = base[l];
        return;
    }
    int mid = (l + r) >> 1;
    seg_build(ls(p), l, mid);
    seg_build(rs(p), mid + 1, r);
    pushup(p);
}

// 单点加：把位置 pos 的值加上 value，沿根到叶子的路径回溯修正
void point_add(int pos, ll value, int p, int l, int r) {
    if (l == r) {
        tr[p] += value;
        return;
    }
    int mid = (l + r) >> 1;
    if (pos <= mid) point_add(pos, value, ls(p), l, mid);
    else point_add(pos, value, rs(p), mid + 1, r);
    pushup(p);
}

// 区间和：把 [ql, qr] 拆成不交的若干节点，逐个累加
ll range_sum(int ql, int qr, int p, int l, int r) {
    if (qr < l || r < ql) return 0;     // 完全不相交
    if (ql <= l && r <= qr) return tr[p];  // 完全被覆盖，直接取
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
            int x;
            ll k;
            cin >> x >> k;
            point_add(x, k, 1, 1, n);
        } else {
            int l, r;
            cin >> l >> r;
            cout << range_sum(l, r, 1, 1, n) << '\n';
        }
    }

    return 0;
}
