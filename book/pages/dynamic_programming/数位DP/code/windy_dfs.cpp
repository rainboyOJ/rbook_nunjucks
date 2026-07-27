#include <bits/stdc++.h>
using namespace std;

// Windy number digit DP.
// A positive integer is windy if the absolute difference between any two
// adjacent digits is at least 2.

int dp[15][10];
int digit[15];

int dfs(int pos, int last, bool limit, bool lead) {
    if (pos == 0) return lead ? 0 : 1;
    if (!limit && !lead && dp[pos][last] != -1) return dp[pos][last];

    int up = limit ? digit[pos] : 9;
    int res = 0;

    for (int cur = 0; cur <= up; ++cur) {
        bool next_limit = limit && (cur == digit[pos]);
        bool next_lead = lead && (cur == 0);

        if (lead) {
            res += dfs(pos - 1, cur, next_limit, next_lead);
        } else if (abs(cur - last) >= 2) {
            res += dfs(pos - 1, cur, next_limit, false);
        }
    }

    if (!limit && !lead) dp[pos][last] = res;
    return res;
}

int solve(int x) {
    if (x <= 0) return 0;

    int len = 0;
    while (x > 0) {
        digit[++len] = x % 10;
        x /= 10;
    }

    return dfs(len, 0, true, true);
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    memset(dp, -1, sizeof(dp));

    int l, r;
    cin >> l >> r;
    cout << solve(r) - solve(l - 1) << '\n';
    return 0;
}
