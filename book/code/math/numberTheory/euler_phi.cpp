#include <bits/stdc++.h>
using namespace std;

long long euler_phi(long long n) {
    long long ans = n;

    for (long long p = 2; p * p <= n; p++) {
        if (n % p != 0) continue;

        ans = ans / p * (p - 1);
        while (n % p == 0) n /= p;
    }

    if (n > 1) ans = ans / n * (n - 1);
    return ans;
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    long long n;
    cin >> n;
    cout << euler_phi(n) << '\n';
    return 0;
}
