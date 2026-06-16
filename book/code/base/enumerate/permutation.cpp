#include <bits/stdc++.h>
using namespace std;

const int maxn = 10 + 5;

int n;
int a[maxn], path[maxn];
bool used[maxn];

// 枚举输入序列的所有排列。
// dep 表示当前正在填写排列中的第 dep 个位置。
void dfs(int dep) {
    if (dep > n) {
        for (int i = 1; i <= n; ++i) {
            cout << path[i] << (i == n ? '\n' : ' ');
        }
        return;
    }

    for (int i = 1; i <= n; ++i) {
        if (used[i]) continue;
        used[i] = true;
        path[dep] = a[i];
        dfs(dep + 1);
        used[i] = false;
    }
}

int main() {
    cin >> n;
    for (int i = 1; i <= n; ++i) cin >> a[i];
    dfs(1);
    return 0;
}
