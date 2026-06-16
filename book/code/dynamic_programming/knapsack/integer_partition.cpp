#include <bits/stdc++.h>
using namespace std;

const int MOD = 1000000007;

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n;
    cin >> n;

    // dp[s] = number of ways to write s as a sum of processed positive integers.
    // Processing part size x from small to large ensures each partition is counted
    // once in nondecreasing part order.
    vector<int> dp(n + 1, 0);
    dp[0] = 1;

    for (int x = 1; x <= n; ++x) {
        for (int s = x; s <= n; ++s) {
            dp[s] += dp[s - x];
            if (dp[s] >= MOD) dp[s] -= MOD;
        }
    }

    cout << dp[n] << '\n';
    return 0;
}
