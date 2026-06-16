#include <bits/stdc++.h>
using namespace std;

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n;
    cin >> n;

    long long first = LLONG_MIN;
    long long second = LLONG_MIN;

    for (int i = 0; i < n; ++i) {
        long long x;
        cin >> x;
        if (x > first) {
            second = first;
            first = x;
        } else if (x > second) {
            second = x;
        }
    }

    cout << first + second << '\n';
    return 0;
}
