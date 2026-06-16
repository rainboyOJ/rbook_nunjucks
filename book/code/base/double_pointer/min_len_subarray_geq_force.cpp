#include <bits/stdc++.h>
using namespace std;

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n;
    long long target;
    cin >> n;

    vector<long long> prefix(n + 1, 0);
    for (int i = 1; i <= n; ++i) {
        long long x;
        cin >> x;
        prefix[i] = prefix[i - 1] + x;
    }
    cin >> target;

    int ans = n + 1;
    for (int left = 1; left <= n; ++left) {
        for (int right = left; right <= n; ++right) {
            long long sum = prefix[right] - prefix[left - 1];
            if (sum >= target) {
                ans = min(ans, right - left + 1);
                break;
            }
        }
    }

    cout << (ans == n + 1 ? 0 : ans) << '\n';
    return 0;
}
