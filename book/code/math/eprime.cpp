#include <bits/stdc++.h>
using namespace std;

vector<int> eratosthenes(int n) {
    vector<bool> is_composite(n + 1, false);
    vector<int> primes;

    for (int i = 2; i <= n; i++) {
        if (is_composite[i]) continue;

        primes.push_back(i);
        if (i > n / i) continue;

        for (int j = i * i; j <= n; j += i) {
            is_composite[j] = true;
        }
    }

    return primes;
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n;
    cin >> n;

    auto primes = eratosthenes(n);
    for (int i = 0; i < (int)primes.size(); i++) {
        if (i) cout << ' ';
        cout << primes[i];
    }
    cout << '\n';

    return 0;
}
