#include <bits/stdc++.h>
using namespace std;

// Count pairs (i, j), i < j, with a[i] == a[j] and j - i <= k.
// The map stores values inside the current left window [j-k, j-1].

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n, k;
    cin >> n >> k;
    vector<long long> a(n);

    unordered_map<long long, long long> cnt;
    long long ans = 0;

    for (int j = 0; j < n; ++j) {
        cin >> a[j];

        if (j > k) {
            long long expired = a[j - k - 1];
            auto it = cnt.find(expired);
            if (it != cnt.end()) {
                --it->second;
                if (it->second == 0) cnt.erase(it);
            }
        }

        auto it = cnt.find(a[j]);
        if (it != cnt.end()) ans += it->second;
        ++cnt[a[j]];
    }

    cout << ans << '\n';
    return 0;
}
