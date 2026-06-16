#include <bits/stdc++.h>
using namespace std;

mt19937 rng((unsigned)chrono::steady_clock::now().time_since_epoch().count());

// 生成 [l, r] 之间的随机整数。
int rnd(int l, int r) {
    return uniform_int_distribution<int>(l, r)(rng);
}

int main() {
    int n = rnd(4, 7);
    cout << n << "\n";
    return 0;
}
