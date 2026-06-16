#include <bits/stdc++.h>
using namespace std;

using i64 = long long;

vector<i64> linear_inverse(int n, i64 mod) {
    vector<i64> inv(n + 1);
    inv[1] = 1;
    for (int i = 2; i <= n; i++) {
        inv[i] = (mod - mod / i) * inv[mod % i] % mod;
    }
    return inv;
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n;
    i64 mod;
    cin >> n >> mod;

    auto inv = linear_inverse(n, mod);
    for (int i = 1; i <= n; i++) {
        cout << inv[i] << '\n';
    }

    return 0;
}
