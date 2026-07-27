#include <bits/stdc++.h>
using namespace std;

// Count pairs (i, j), i < j, whose sorted value difference is in [low, high].
// Duplicates are counted by index pairs.
int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n;
    long long low, high;
    cin >> n;

    vector<long long> a(n);
    for (int i = 0; i < n; ++i) cin >> a[i];
    cin >> low >> high;

    sort(a.begin(), a.end());

    long long ans = 0;
    int first_ge_low = 1;
    int first_gt_high = 1;

    for (int i = 0; i < n; ++i) {
        first_ge_low = max(first_ge_low, i + 1);
        first_gt_high = max(first_gt_high, i + 1);

        while (first_ge_low < n && a[first_ge_low] - a[i] < low) {
            ++first_ge_low;
        }
        while (first_gt_high < n && a[first_gt_high] - a[i] <= high) {
            ++first_gt_high;
        }

        ans += first_gt_high - first_ge_low;
    }

    cout << ans << '\n';
    return 0;
}
