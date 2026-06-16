#include <bits/stdc++.h>
using namespace std;

const int maxn = 10 + 5;

int n, m;
int path[maxn];

// dep 表示当前正在填写第 dep 个位置。
// 每个位置都可以独立选择 0, 1, ..., m - 1。
void dfs(int dep) {
    if (dep > n) {
        for (int i = 1; i <= n; ++i) {
            cout << path[i] << (i == n ? '\n' : ' ');
        }
        return;
    }

    for (int x = 0; x < m; ++x) {
        path[dep] = x;
        dfs(dep + 1);
    }
}

int main() {
    cin >> n >> m;
    dfs(1);
    return 0;
}
