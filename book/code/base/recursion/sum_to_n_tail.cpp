#include <bits/stdc++.h>
using namespace std;

// calc(a, s): 已经累加到 a - 1，当前和是 s，接下来处理 a。
int calc(int a, int n, int s) {
    if (a == n + 1) return s;
    return calc(a + 1, n, s + a);
}

int main() {
    int n;
    cin >> n;
    cout << calc(1, n, 0) << '\n';
    return 0;
}
