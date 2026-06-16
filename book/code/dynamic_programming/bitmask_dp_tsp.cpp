#include <bits/stdc++.h>
using namespace std;

const long long INF = (1LL << 60);

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n;
    cin >> n;

    vector<vector<long long>> cost(n, vector<long long>(n));
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            cin >> cost[i][j];
        }
    }

    int limit = 1 << n;
    vector<vector<long long>> dp(limit, vector<long long>(n, INF));
    dp[1][0] = 0;

    for (int mask = 1; mask < limit; mask++) {
        for (int last = 0; last < n; last++) {
            if (dp[mask][last] == INF) continue;
            if ((mask & (1 << last)) == 0) continue;

            for (int next = 0; next < n; next++) {
                if (mask & (1 << next)) continue;
                int new_mask = mask | (1 << next);
                dp[new_mask][next] = min(dp[new_mask][next], dp[mask][last] + cost[last][next]);
            }
        }
    }

    long long answer = INF;
    int full = limit - 1;
    for (int last = 0; last < n; last++) {
        answer = min(answer, dp[full][last]);
    }

    cout << answer << '\n';
    return 0;
}
