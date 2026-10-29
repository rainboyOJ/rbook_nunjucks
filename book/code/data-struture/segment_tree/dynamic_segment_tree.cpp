#include <bits/stdc++.h>
using namespace std;
using ll = long long;

// 动态开点线段树：值域可以很大（如 1e9），只在真正访问到的位置分配节点。
// 空间 O(m log V)：m 次修改，每次最多新增 log V 个节点。
// 与普通线段树相比，这类题的数组没法按值域直接开，只能靠节点池。
const int MAXNODE = 3000005;  // 节点池：按 m log V 估，再留一点余量

int lc[MAXNODE];  // lc[p]：p 的左儿子下标，0 表示空
int rc[MAXNODE];  // rc[p]：p 的右儿子下标，0 表示空
ll sum[MAXNODE];  // sum[p]：p 管辖区间内所有数之和
int root = 0;     // 根节点下标，0 表示空树
int tot = 0;      // 已分配的节点数，0 号节点固定为空节点

// 新建一个空节点并返回它的下标
int new_node() {
    ++tot;
    lc[tot] = rc[tot] = 0;
    sum[tot] = 0;
    return tot;
}

// 单点加：在值域区间 [l, r] 上把 x 位置加上 value。
// p 用引用传参，节点为空时在这里分配，新编号会直接写回父节点的孩子。
void update(int &p, ll l, ll r, ll x, ll value) {
    if (!p) p = new_node();  // 走到空节点就开一个，这是“动态开点”
    if (l == r) {
        sum[p] += value;
        return;
    }
    ll mid = l + ((r - l) >> 1);  // 防溢出的中点写法
    if (x <= mid) update(lc[p], l, mid, x, value);
    else update(rc[p], mid + 1, r, x, value);
    sum[p] = sum[lc[p]] + sum[rc[p]];
}

// 区间和：节点为空说明整段都是 0，直接返回，绝不在这里开点
ll query(int p, ll l, ll r, ll ql, ll qr) {
    if (!p || qr < l || r < ql) return 0;
    if (ql <= l && r <= qr) return sum[p];
    ll mid = l + ((r - l) >> 1);
    ll answer = query(lc[p], l, mid, ql, qr);
    answer += query(rc[p], mid + 1, r, ql, qr);
    return answer;
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    // 例题：值域 [1, 1e9] 上的单点加与区间求和
    ll limit = 1000000000;

    update(root, 1, limit, 1000, 5);       // 在位置 1000 加上 5
    update(root, 1, limit, 2000000, 10);   // 在位置 2000000 加上 10
    update(root, 1, limit, 999999999, 20); // 在位置 999999999 加上 20

    // [1, 10000] 只包含位置 1000，所以答案是 5
    cout << query(root, 1, limit, 1, 10000) << '\n';
    // 整个值域包含全部修改，所以答案是 5 + 10 + 20 = 35
    cout << query(root, 1, limit, 1, limit) << '\n';
    // 在 1e9 的值域上只用了这么点节点，正好说明动态开点省空间
    cout << "Allocated nodes: " << tot << '\n';

    return 0;
}
