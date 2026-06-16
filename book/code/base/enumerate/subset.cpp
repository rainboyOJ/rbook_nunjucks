#include <bits/stdc++.h>
using namespace std;

const int maxn = 30 + 5;

int n;
int a[maxn], path[maxn];

// 枚举所有子集。
// 每次进入 dfs 都输出当前已经选择的元素。
void dfs(int dep, int last) {
    for (int i = 1; i < dep; ++i) {
        cout << path[i] << (i + 1 == dep ? '\n' : ' ');
    }
    if (dep == 1) cout << "\n";  // 空集

    for (int i = last + 1; i <= n; ++i) {
        path[dep] = a[i];
        dfs(dep + 1, i);
    }
}

int main() {
    cin >> n;
    for (int i = 1; i <= n; ++i) cin >> a[i];
    dfs(1, 0);
    return 0;
}
