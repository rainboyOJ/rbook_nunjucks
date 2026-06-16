#include <bits/stdc++.h>
using namespace std;

struct LinearSieve {
    vector<int> primes;
    vector<int> min_factor;
    vector<bool> is_composite;

    void build(int n) {
        primes.clear();
        min_factor.assign(n + 1, 0);
        is_composite.assign(n + 1, false);

        for (int i = 2; i <= n; i++) {
            if (!is_composite[i]) {
                primes.push_back(i);
                min_factor[i] = i;
            }

            for (int p : primes) {
                if (p > n / i) break;
                int x = i * p;
                is_composite[x] = true;
                min_factor[x] = p;

                // p 是 i 的最小质因子时，不能再用更大的质数去生成 i*p'。
                if (i % p == 0) break;
            }
        }
    }

    bool is_prime(int x) const {
        return x >= 2 && !is_composite[x];
    }
};

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n;
    cin >> n;

    LinearSieve sieve;
    sieve.build(n);

    for (int i = 0; i < (int)sieve.primes.size(); i++) {
        if (i) cout << ' ';
        cout << sieve.primes[i];
    }
    cout << '\n';

    return 0;
}
