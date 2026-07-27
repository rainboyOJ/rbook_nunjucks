#include <bits/stdc++.h>
using namespace std;

int main() {
    int n, capacity;
    cin >> n >> capacity;

    vector<int> weight(n + 1), value(n + 1);
    for (int i = 1; i <= n; i++) {
        cin >> weight[i] >> value[i];
    }

    vector<vector<int>> dp(n + 1, vector<int>(capacity + 1, 0));

    for (int i = 1; i <= n; i++) {
        for (int c = 0; c <= capacity; c++) {
            dp[i][c] = dp[i - 1][c];
            if (c >= weight[i]) {
                dp[i][c] = max(dp[i][c],
                               dp[i][c - weight[i]] + value[i]);
            }
        }
    }

    cout << dp[n][capacity] << "\n";
    return 0;
}
