#include <bits/stdc++.h>
using namespace std;

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n, m;
    cin >> n >> m;

    vector<int> a(n + 1);
    for (int i = 1; i <= n; ++i) cin >> a[i];

    while (m--) {
        int x;
        cin >> x;

        int pos = n + 1;
        for (int i = 1; i <= n; ++i) {
            if (a[i] >= x) {
                pos = i;
                break;
            }
        }

        if (pos == n + 1) cout << "not found\n";
        else cout << a[pos] << ' ' << pos << '\n';
    }

    return 0;
}
