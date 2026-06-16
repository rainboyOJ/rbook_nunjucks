#include <bits/stdc++.h>
using namespace std;

const int maxn = 30 + 5;

int n, m;
int a[maxn], path[maxn];

// 从 n 个元素中选 m 个。
// dep 表示当前要选第 dep 个数，last 表示上一次选择的位置。
void dfs(int dep, int last) {
    if (dep > m) {
        for (int i = 1; i <= m; ++i) {
            cout << path[i] << (i == m ? '\n' : ' ');
        }
        return;
    }

    // 剩余位置不足时可以提前停止。
    for (int i = last + 1; i <= n - (m - dep); ++i) {
        path[dep] = a[i];
        dfs(dep + 1, i);
    }
}

int main() {
    cin >> n >> m;
    for (int i = 1; i <= n; ++i) cin >> a[i];
    dfs(1, 0);
    return 0;
}
