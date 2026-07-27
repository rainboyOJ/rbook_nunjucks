#include <bits/stdc++.h>
using namespace std;

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n;
    cin >> n;

    long long positive_sum = 0;
    long long negative_abs_sum = 0;

    for (int i = 0; i < n; ++i) {
        long long x;
        cin >> x;
        if (x > 0) positive_sum += x;
        else negative_abs_sum += -x;
    }

    cout << max(positive_sum, negative_abs_sum) << '\n';
    return 0;
}
