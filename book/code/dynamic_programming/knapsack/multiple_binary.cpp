#include <bits/stdc++.h>
using namespace std;

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n, capacity;
    cin >> n >> capacity;

    vector<int> dp(capacity + 1, 0);

    for (int i = 1; i <= n; ++i) {
        int weight, value, amount;
        cin >> weight >> value >> amount;

        for (int block = 1; amount > 0; block <<= 1) {
            int cnt = min(block, amount);
            amount -= cnt;

            int pack_weight = cnt * weight;
            int pack_value = cnt * value;

            for (int c = capacity; c >= pack_weight; --c) {
                dp[c] = max(dp[c], dp[c - pack_weight] + pack_value);
            }
        }
    }

    cout << dp[capacity] << '\n';
    return 0;
}
