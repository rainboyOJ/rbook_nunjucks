#include <bits/stdc++.h>
using namespace std;

// 种类并查集（并查集判二分图）竞赛精简版：每个点拆成 x 与 x+n（对立面）
// 输入 n m 与 m 条边 x y（x 与 y 必须不同类），输出 YES/NO
// 口诀：要求不同类，就把 x 挂到 y 的对立面、y 挂到 x 的对立面
// 易错点：拆点后容量翻倍，init 要开到 2n；x 与 y 同集合才是矛盾，合并本身不会矛盾
// 和别的模板一起用时 maxn 只留一份

const int maxn = 1e6 + 5;
int fa[maxn * 2], sz[maxn * 2];  // 拆点后下标 1..2n

void init(int n) {
    for (int i = 1; i <= n; i++) fa[i] = i, sz[i] = 1;
}

int find(int x) {
    return fa[x] == x ? x : fa[x] = find(fa[x]);
}

// 小树挂大树
void unite(int x, int y) {
    int fx = find(x), fy = find(y);
    if (fx == fy) return;
    if (sz[fx] < sz[fy]) swap(fx, fy);
    fa[fy] = fx;
    sz[fx] += sz[fy];
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    int n, m;
    cin >> n >> m;
    init(2 * n);  // 拆点：i 是点 i，i+n 是它的对立面
    bool ok = true;
    while (m--) {
        int x, y;
        cin >> x >> y;
        if (find(x) == find(y)) ok = false;  // 同类却要求不同类：矛盾
        unite(x, y + n);  // x 和 y 的对立面同类
        unite(x + n, y);  // x 的对立面和 y 同类
    }
    cout << (ok ? "YES" : "NO") << "\n";
    return 0;
}
