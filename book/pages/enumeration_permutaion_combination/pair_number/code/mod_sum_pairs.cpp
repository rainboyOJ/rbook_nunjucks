#include <bits/stdc++.h>
using namespace std;

// Count pairs (i, j), i < j, with (a[i] + a[j]) % mod == 0.

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n, mod;
    cin >> n >> mod;

    vector<long long> cnt(mod, 0);
    long long ans = 0;

    for (int i = 0; i < n; ++i) {
        long long x;
        cin >> x;
        int r = (int)((x % mod + mod) % mod);
        int need = (mod - r) % mod;
        ans += cnt[need];
        ++cnt[r];
    }

    cout << ans << '\n';
    return 0;
}
