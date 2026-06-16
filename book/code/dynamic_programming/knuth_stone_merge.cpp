#include <bits/stdc++.h>
using namespace std;

const long long INF = (1LL << 62);

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n;
    cin >> n;

    vector<long long> a(n + 1), prefix(n + 1, 0);
    for (int i = 1; i <= n; i++) {
        cin >> a[i];
        prefix[i] = prefix[i - 1] + a[i];
    }

    auto sum = [&](int l, int r) {
        return prefix[r] - prefix[l - 1];
    };

    vector<vector<long long>> dp(n + 2, vector<long long>(n + 2, 0));
    vector<vector<int>> opt(n + 2, vector<int>(n + 2, 0));

    for (int i = 1; i <= n; i++) {
        opt[i][i] = i;
    }

    for (int len = 2; len <= n; len++) {
        for (int l = 1; l + len - 1 <= n; l++) {
            int r = l + len - 1;
            dp[l][r] = INF;

            int left = opt[l][r - 1];
            int right = opt[l + 1][r];
            left = max(left, l);
            right = min(right, r - 1);

            for (int k = left; k <= right; k++) {
                long long cur = dp[l][k] + dp[k + 1][r] + sum(l, r);
                if (cur < dp[l][r]) {
                    dp[l][r] = cur;
                    opt[l][r] = k;
                }
            }
        }
    }

    cout << dp[1][n] << '\n';
    return 0;
}
