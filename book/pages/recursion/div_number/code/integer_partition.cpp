#include <bits/stdc++.h>
using namespace std;

const int maxn = 500 + 5;

long long memo[maxn][maxn];

// f(n, m): 把 n 划分成若干个正整数之和，且每个数不超过 m 的方案数。
long long f(int n, int m) {
    if (n == 0) return 1;
    if (m == 0) return 0;
    if (m > n) return f(n, n);
    if (memo[n][m] != -1) return memo[n][m];

    // 不使用 m 的方案 + 至少使用一个 m 的方案。
    memo[n][m] = f(n, m - 1) + f(n - m, m);
    return memo[n][m];
}

int main() {
    int n;
    cin >> n;
    memset(memo, -1, sizeof(memo));
    cout << f(n, n) << '\n';
    return 0;
}
