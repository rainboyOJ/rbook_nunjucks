#include <bits/stdc++.h>
using namespace std;

// Minimum length of a contiguous subarray with sum >= target.
// The array must contain positive integers.
int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n;
    long long target;
    cin >> n;

    vector<long long> a(n + 1);
    for (int i = 1; i <= n; ++i) cin >> a[i];
    cin >> target;

    int ans = n + 1;
    int left = 1;
    long long sum = 0;

    for (int right = 1; right <= n; ++right) {
        sum += a[right];
        while (left <= right && sum >= target) {
            ans = min(ans, right - left + 1);
            sum -= a[left];
            ++left;
        }
    }

    cout << (ans == n + 1 ? 0 : ans) << '\n';
    return 0;
}
