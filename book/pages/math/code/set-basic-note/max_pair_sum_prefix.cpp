#include <bits/stdc++.h>
using namespace std;

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n;
    cin >> n;

    vector<long long> a(n + 1);
    for (int i = 1; i <= n; ++i) cin >> a[i];

    long long prefix_max = a[1];
    long long ans = LLONG_MIN;

    for (int i = 2; i <= n; ++i) {
        ans = max(ans, prefix_max + a[i]);
        prefix_max = max(prefix_max, a[i]);
    }

    cout << ans << '\n';
    return 0;
}
