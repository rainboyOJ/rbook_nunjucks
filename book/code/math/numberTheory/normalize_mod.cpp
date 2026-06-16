#include <bits/stdc++.h>
using namespace std;

long long normalize_mod(long long a, long long m) {
    m = llabs(m);
    long long r = a % m;
    if (r < 0) r += m;
    return r;
}

long long move_on_circle(long long pos, long long step, long long n) {
    return normalize_mod(pos + step, n);
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    long long a, m;
    cin >> a >> m;
    cout << normalize_mod(a, m) << '\n';

    long long pos, step, n;
    if (cin >> pos >> step >> n) {
        cout << move_on_circle(pos, step, n) << '\n';
    }

    return 0;
}
