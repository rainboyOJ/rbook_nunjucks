#include <iostream>
#include <vector>
using namespace std;

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n;
    cin >> n;

    int limit = 1 << n;
    vector<long long> f(limit);
    for (int mask = 0; mask < limit; ++mask) {
        cin >> f[mask];
    }

    // f[mask] = sum of original f[sup] for every sup ⊇ mask.
    for (int bit = 0; bit < n; ++bit) {
        for (int mask = 0; mask < limit; ++mask) {
            if ((mask & (1 << bit)) == 0) {
                f[mask] += f[mask ^ (1 << bit)];
            }
        }
    }

    for (int mask = 0; mask < limit; ++mask) {
        if (mask) cout << ' ';
        cout << f[mask];
    }
    cout << '\n';

    return 0;
}
