#include <bits/stdc++.h>
using namespace std;

int main() {
    int n, capacity;
    cin >> n >> capacity;

    vector<int> dp(capacity + 1, 0);

    for (int i = 1; i <= n; i++) {
        int weight, value;
        cin >> weight >> value;

        for (int c = weight; c <= capacity; c++) {
            dp[c] = max(dp[c], dp[c - weight] + value);
        }
    }

    cout << dp[capacity] << "\n";
    return 0;
}
