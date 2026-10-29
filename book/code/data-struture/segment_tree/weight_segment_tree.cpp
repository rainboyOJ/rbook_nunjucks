#include <bits/stdc++.h>
using namespace std;

// 权值线段树（动态开点）：按值域统计每个数出现的次数，
// 支持插入、删除、排名、第 k 小、前驱与后继。
const int maxn = 2000000 + 5;  // 节点池大小：按操作次数 * log(值域) 估

const int MINV = -10000000;  // 值域下界
const int MAXV = 10000000;   // 值域上界

int lc[maxn];    // lc[p]：p 的左儿子下标，0 表示空
int rc[maxn];    // rc[p]：p 的右儿子下标，0 表示空
int cnt[maxn];   // cnt[p]：p 管辖区间内的元素个数
int root = 0;    // 根节点下标，0 表示空树
int tot = 0;     // 已分配的节点数，0 号节点固定为空节点

// 新建一个空节点并返回它的下标
int new_node() {
    ++tot;
    lc[tot] = rc[tot] = cnt[tot] = 0;
    return tot;
}

// 在值域区间 [l, r] 的节点 p 上把 pos 的个数增加 delta
void update(int &p, int l, int r, int pos, int delta) {
    if (!p) p = new_node();  // 走到空节点就开一个，这是“动态开点”
    cnt[p] += delta;
    if (l == r) return;
    int mid = l + ((r - l) >> 1);  // 防溢出的中点写法
    if (pos <= mid) update(lc[p], l, mid, pos, delta);
    else update(rc[p], mid + 1, r, pos, delta);
}

// 区间计数：值域 [ql, qr] 内一共有多少个数
int query(int p, int l, int r, int ql, int qr) {
    if (!p || qr < l || r < ql) return 0;  // 空节点整段都是 0
    if (ql <= l && r <= qr) return cnt[p];
    int mid = l + ((r - l) >> 1);
    int answer = query(lc[p], l, mid, ql, qr);
    answer += query(rc[p], mid + 1, r, ql, qr);
    return answer;
}

// 小于 x 的元素个数 = 值域 [MINV, x-1] 的计数
int count_less(int x) {
    if (x <= MINV) return 0;
    return query(root, MINV, MAXV, MINV, x - 1);
}

// 第 k 小：看左儿子的个数够不够，够就往左走，否则往右并扣掉左半段
int kth(int p, int l, int r, int k) {
    if (l == r) return l;
    int mid = l + ((r - l) >> 1);
    if (k <= cnt[lc[p]]) return kth(lc[p], l, mid, k);
    return kth(rc[p], mid + 1, r, k - cnt[lc[p]]);
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n;
    cin >> n;
    while (n--) {
        int op, x;
        cin >> op >> x;
        if (op == 1) {
            update(root, MINV, MAXV, x, 1);           // 插入
        } else if (op == 2) {
            update(root, MINV, MAXV, x, -1);          // 删除
        } else if (op == 3) {
            cout << count_less(x) + 1 << '\n';        // 排名
        } else if (op == 4) {
            cout << kth(root, MINV, MAXV, x) << '\n'; // 第 k 小
        } else if (op == 5) {
            cout << kth(root, MINV, MAXV, count_less(x)) << '\n';  // 前驱
        } else {
            // 后继：大于 x 的最小数 = 第 (≤x 的个数 + 1) 小
            int leq = count_less(x + 1);
            cout << kth(root, MINV, MAXV, leq + 1) << '\n';
        }
    }

    return 0;
}
