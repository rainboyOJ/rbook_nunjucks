#include <bits/stdc++.h>
using namespace std;

// f(n) 表示 1 + 2 + ... + n。
int sum_to_n(int n) {
    if (n == 1) return 1;
    return n + sum_to_n(n - 1);
}

int main() {
    int n;
    cin >> n;
    cout << sum_to_n(n) << '\n';
    return 0;
}
