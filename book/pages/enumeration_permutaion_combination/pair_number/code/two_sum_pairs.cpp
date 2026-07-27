#include <bits/stdc++.h>
using namespace std;

// Count pairs (i, j), i < j, with a[i] + a[j] == target.

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n;
    long long target;
    cin >> n >> target;

    unordered_map<long long, long long> cnt;
    long long ans = 0;

    for (int i = 0; i < n; ++i) {
        long long x;
        cin >> x;
        auto it = cnt.find(target - x);
        if (it != cnt.end()) ans += it->second;
        ++cnt[x];
    }

    cout << ans << '\n';
    return 0;
}
