#include <bits/stdc++.h>
using namespace std;

const int maxn = 100000 + 5;

long long memo[maxn];

long long fibonacci(int n) {
    if (n == 1 || n == 2) return 1;
    if (memo[n] != 0) return memo[n];
    memo[n] = fibonacci(n - 1) + fibonacci(n - 2);
    return memo[n];
}

int main() {
    int n;
    cin >> n;
    cout << fibonacci(n) << '\n';
    return 0;
}
