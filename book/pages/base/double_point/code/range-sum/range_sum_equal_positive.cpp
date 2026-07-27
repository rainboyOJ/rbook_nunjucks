#include <bits/stdc++.h>
using namespace std;

// Print all 1-indexed intervals whose sum is exactly target.
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

    int left = 1;
    long long sum = 0;

    for (int right = 1; right <= n; ++right) {
        sum += a[right];

        while (left <= right && sum > target) {
            sum -= a[left];
            ++left;
        }

        if (sum == target) {
            cout << left << ' ' << right << '\n';
        }
    }

    return 0;
}
