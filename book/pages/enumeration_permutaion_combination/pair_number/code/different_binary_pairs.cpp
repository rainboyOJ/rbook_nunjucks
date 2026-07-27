#include <bits/stdc++.h>
using namespace std;

// Count pairs (i, j), i < j, with different binary values.

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n;
    cin >> n;

    long long cnt[2] = {0, 0};
    long long ans = 0;

    for (int i = 0; i < n; ++i) {
        int x;
        cin >> x;
        ans += cnt[x ^ 1];
        ++cnt[x];
    }

    cout << ans << '\n';
    return 0;
}
