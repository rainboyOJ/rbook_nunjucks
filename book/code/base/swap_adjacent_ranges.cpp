#include <bits/stdc++.h>
using namespace std;

template <class Iterator>
void swap_adjacent_ranges(Iterator first, Iterator middle, Iterator last) {
    reverse(first, middle);
    reverse(middle, last);
    reverse(first, last);
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n, l, m, r;
    cin >> n;
    vector<int> a(n);
    for (int &x : a) cin >> x;

    // 交换 0-indexed 半开区间 [l, m) 和 [m, r)。
    cin >> l >> m >> r;
    swap_adjacent_ranges(a.begin() + l, a.begin() + m, a.begin() + r);

    for (int i = 0; i < n; i++) {
        if (i) cout << ' ';
        cout << a[i];
    }
    cout << '\n';

    return 0;
}
