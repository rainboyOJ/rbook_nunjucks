#include <bits/stdc++.h>
using namespace std;

using i64 = long long;

i64 ceil_div(i64 a, i64 b) {
    return a / b + (a % b != 0);
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    i64 a, b;
    cin >> a >> b;
    cout << ceil_div(a, b) << '\n';
    return 0;
}
