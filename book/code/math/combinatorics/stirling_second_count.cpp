#include <bits/stdc++.h>
using namespace std;

// Stirling number of the second kind:
// S(n, m) = ways to split n distinct objects into m non-empty identical boxes.
//
// Recurrence:
//   S(n, m) = S(n - 1, m - 1) + m * S(n - 1, m)

using int64 = long long;

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n, m;
    cin >> n >> m;

    vector<vector<int64>> dp(n + 1, vector<int64>(m + 1, 0));
    dp[0][0] = 1;

    for (int i = 1; i <= n; ++i) {
        for (int j = 1; j <= min(i, m); ++j) {
            dp[i][j] = dp[i - 1][j - 1] + j * dp[i - 1][j];
        }
    }

    cout << dp[n][m] << '\n';
    return 0;
}
