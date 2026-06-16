#include <bits/stdc++.h>
using namespace std;

// Count pairs (i, j), i < j, whose sorted value difference is in [low, high].
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
    for (int i = 0; i < n; ++i) {
        for (int j = i + 1; j < n; ++j) {
            long long diff = a[j] - a[i];
            if (low <= diff && diff <= high) ++ans;
        }
    }

    cout << ans << '\n';
    return 0;
}
