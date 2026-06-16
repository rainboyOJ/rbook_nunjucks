#include <bits/stdc++.h>
using namespace std;

int main() {
    int n;
    cin >> n;

    vector<vector<int>> a(n + 1, vector<int>(n + 2, 0));
    for (int i = 1; i <= n; i++) {
        for (int j = 1; j <= i; j++) {
            cin >> a[i][j];
        }
    }

    vector<int> dp(n + 2, 0);
    for (int j = 1; j <= n; j++) {
        dp[j] = a[n][j];
    }

    for (int i = n - 1; i >= 1; i--) {
        for (int j = 1; j <= i; j++) {
            dp[j] = max(dp[j], dp[j + 1]) + a[i][j];
        }
    }

    cout << dp[1] << "\n";
    return 0;
}
