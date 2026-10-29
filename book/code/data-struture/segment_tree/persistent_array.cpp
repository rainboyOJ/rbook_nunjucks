#include <bits/stdc++.h>
using namespace std;
using ll = long long;

// 可持久化数组：每次修改只复制从根到该叶子的那一条链，
// 于是得到一个新版本，旧版本的所有节点原封不动，仍然可查。
const int maxn = 100000 + 5;   // 数组长度上限
const int MAXNODE = 4000000 + 5;  // 节点池大小：约 (n + m * log n)

int lc[MAXNODE];  // lc[p]：p 的左儿子下标，0 表示空
int rc[MAXNODE];  // rc[p]：p 的右儿子下标，0 表示空
ll val[MAXNODE];  // val[p]：叶子节点存的数组值
int tot = 0;      // 已分配的节点数，0 号节点固定为空节点

// 复制节点 p（左右儿子与值原样照抄），返回新节点的下标
int clone(int p) {
    ++tot;
    lc[tot] = lc[p];
    rc[tot] = rc[p];
    val[tot] = val[p];
    return tot;
}

// 用 a[1..n] 建树，返回根节点下标
int seg_build(int l, int r, const ll *a) {
    int p = clone(0);
    if (l == r) {
        val[p] = a[l];
        return p;
    }
    int mid = (l + r) >> 1;
    lc[p] = seg_build(l, mid, a);
    rc[p] = seg_build(mid + 1, r, a);
    return p;
}

// 在版本 p 上把位置 pos 改成 value，返回新版本的根下标
int update(int p, int l, int r, int pos, ll value) {
    int q = clone(p);  // 只复制这一个节点，没走到的子树继续和老版本共享
    if (l == r) {
        val[q] = value;
        return q;
    }
    int mid = (l + r) >> 1;
    if (pos <= mid) lc[q] = update(lc[p], l, mid, pos, value);
    else rc[q] = update(rc[p], mid + 1, r, pos, value);
    return q;
}

// 查询版本 p 中位置 pos 的值
ll query(int p, int l, int r, int pos) {
    if (l == r) return val[p];
    int mid = (l + r) >> 1;
    if (pos <= mid) return query(lc[p], l, mid, pos);
    return query(rc[p], mid + 1, r, pos);
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n, m;
    cin >> n >> m;

    static ll a[maxn];
    for (int i = 1; i <= n; i++) cin >> a[i];

    // root[v]：第 v 个版本的根下标，版本 0 是初始版本
    static int root[maxn];
    root[0] = seg_build(1, n, a);

    for (int i = 1; i <= m; i++) {
        int version, op, pos;
        cin >> version >> op >> pos;
        if (op == 1) {
            ll value;
            cin >> value;
            root[i] = update(root[version], 1, n, pos, value);
        } else {
            root[i] = root[version];  // 查询也占一个版本号，内容与来源版本相同
            cout << query(root[version], 1, n, pos) << '\n';
        }
    }

    return 0;
}
