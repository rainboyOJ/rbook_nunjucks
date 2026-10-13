#include <bits/stdc++.h>
using namespace std;

// 并查集竞赛精简版（不用 struct）：路径压缩 + 按大小合并
// 输入 n m，接下来 m 行操作：1 x y 合并，2 x y 输出是否同集合（Y/N）
// 口诀：find 找根压到底，unite 小树挂大树
// 易错点：init 别只写 fa[i] = i，sz[i] 也要置 1；合并后 sz 只在新根上有效
// 和别的模板一起用时 maxn 只留一份

const int maxn = 1e6 + 5;
int fa[maxn], sz[maxn];  // sz[x] 仅在根上有效

void init(int n) {
    for (int i = 1; i <= n; i++) fa[i] = i, sz[i] = 1;
}

// 查 x 的根，顺手把整条链压到根上
int find(int x) {
    return fa[x] == x ? x : fa[x] = find(fa[x]);
}

bool same(int x, int y) { return find(x) == find(y); }

// 小树挂大树，返回是否真的发生了合并
bool unite(int x, int y) {
    int fx = find(x), fy = find(y);
    if (fx == fy) return false;
    if (sz[fx] < sz[fy]) swap(fx, fy);
    fa[fy] = fx;
    sz[fx] += sz[fy];
    return true;
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    int n, m;
    cin >> n >> m;
    init(n);
    while (m--) {
        int op, x, y;
        cin >> op >> x >> y;
        if (op == 1) unite(x, y);
        else cout << (same(x, y) ? "Y" : "N") << "\n";
    }
    return 0;
}
