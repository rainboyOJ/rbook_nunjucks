#include <bits/stdc++.h>
using namespace std;

// Floyd 全源最短路竞赛精简版
// 输入 n m 与 m 条边 u v w，输出任意两点最短路矩阵，不可达输出 INF
// 口诀：中转点 k 必须在最外层；d[i][k] 不可达就 continue，防溢出

const int N = 410;          // 最大点数，按题目改
const long long INF = 1e18; // 大于最长路即可
long long d[N][N];
int n, m;

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cin >> n >> m;
    for (int i = 1; i <= n; i++)
        for (int j = 1; j <= n; j++) d[i][j] = (i == j ? 0 : INF);
    for (int i = 0; i < m; i++) {
        int u, v;
        long long w;
        cin >> u >> v >> w;
        d[u][v] = min(d[u][v], w); // 重边取最小
    }
    for (int k = 1; k <= n; k++) // 中转点最外层，顺序不能换
        for (int i = 1; i <= n; i++) {
            if (d[i][k] == INF) continue;
            for (int j = 1; j <= n; j++)
                if (d[k][j] != INF) d[i][j] = min(d[i][j], d[i][k] + d[k][j]);
        }
    for (int i = 1; i <= n; i++) {
        for (int j = 1; j <= n; j++) {
            if (d[i][j] == INF) cout << "INF";
            else cout << d[i][j];
            cout << " \n"[j == n];
        }
    }
    return 0;
}
