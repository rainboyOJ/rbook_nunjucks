#include <bits/stdc++.h>
using namespace std;

int main() {
    int n, capacity;
    cin >> n >> capacity;

    const int NEG_INF = -1e9;
    vector<int> dp(capacity + 1, NEG_INF);
    dp[0] = 0;

    for (int i = 1; i <= n; i++) {
        int weight, value;
        cin >> weight >> value;

        for (int c = capacity; c >= weight; c--) {
            if (dp[c - weight] == NEG_INF) continue;
            dp[c] = max(dp[c], dp[c - weight] + value);
        }
    }

    if (dp[capacity] == NEG_INF) {
        cout << "Impossible\n";
    } else {
        cout << dp[capacity] << "\n";
    }

    return 0;
}
