#include <bits/stdc++.h>
using namespace std;

// 区间最小值线段树（单点取 min 更新），节点与区间均为 1 下标。
const int maxn = 100000 + 5;  // 按题目规模调整，n 不能超过 maxn
const int INF = 1e9;          // 叶子初值，也是查询无解时的哨兵

int tr[maxn << 2];  // tr[p]：节点 p 管辖区间的最小值

int ls(int p) { return p << 1; }     // p 的左儿子 2p
int rs(int p) { return p << 1 | 1; } // p 的右儿子 2p+1

// 上推：父区间的最小值 = 两个儿子的较小值
void pushup(int p) { tr[p] = min(tr[ls(p)], tr[rs(p)]); }

// 建树：所有叶子先置成 INF，表示这些位置还没有任何值
void seg_build(int p, int l, int r) {
    tr[p] = INF;
    if (l == r) return;
    int mid = (l + r) >> 1;
    seg_build(ls(p), l, mid);
    seg_build(rs(p), mid + 1, r);
}

// 单点取 min：把位置 pos 的值更新为 min(原值, value)
void update_min(int pos, int value, int p, int l, int r) {
    if (l == r) {
        tr[p] = min(tr[p], value);
        return;
    }
    int mid = (l + r) >> 1;
    if (pos <= mid) update_min(pos, value, ls(p), l, mid);
    else update_min(pos, value, rs(p), mid + 1, r);
    pushup(p);
}

// 区间最小值：空区间返回 INF
int query_min(int ql, int qr, int p, int l, int r) {
    if (ql > qr || qr < l || r < ql) return INF;
    if (ql <= l && r <= qr) return tr[p];
    int mid = (l + r) >> 1;
    int answer = query_min(ql, qr, ls(p), l, mid);
    answer = min(answer, query_min(ql, qr, rs(p), mid + 1, r));
    return answer;
}

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

    // 值域离散化：线段树建在数值上，叶子存该值第一次出现的位置
    sort(values.begin(), values.end());
    values.erase(unique(values.begin(), values.end()), values.end());
    int vn = (int)values.size();
    seg_build(1, 1, vn);

    for (int i = 1; i <= n; i++) {
        int rank = lower_bound(values.begin(), values.end(), a[i]) - values.begin() + 1;
        update_min(rank, i, 1, 1, vn);  // 从左往右扫描，叶子只会越来越小
    }

    while (q--) {
        int x;
        cin >> x;
        // 所有小于 x 的值，正好落在压缩值域的 [1, last_less]
        int last_less = lower_bound(values.begin(), values.end(), x) - values.begin();
        int pos = query_min(1, last_less, 1, 1, vn);
        if (pos == INF) cout << n + 1 << '\n';  // 不存在 a[i] < x
        else cout << pos << '\n';
    }

    return 0;
}
