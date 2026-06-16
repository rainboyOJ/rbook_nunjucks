#include <bits/stdc++.h>
using namespace std;

using ll = long long;

// 计算 base^exp mod mod，要求 mod > 0。
ll quick_pow(ll base, ll exp, ll mod) {
    ll ans = 1 % mod;
    base %= mod;

    while (exp > 0) {
        if (exp & 1) ans = ans * base % mod;
        base = base * base % mod;
        exp >>= 1;
    }

    return ans;
}

int main() {
    ll base, exp, mod;
    cin >> base >> exp >> mod;
    cout << quick_pow(base, exp, mod) << "\n";
    return 0;
}
