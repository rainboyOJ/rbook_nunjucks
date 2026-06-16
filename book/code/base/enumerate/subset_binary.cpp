#include <bits/stdc++.h>
using namespace std;

const int maxn = 30 + 5;

int n;
int a[maxn];

// 用二进制状态枚举所有子集。
// mask 的第 i 位为 1，表示选择 a[i + 1]。
int main() {
    cin >> n;
    for (int i = 1; i <= n; ++i) cin >> a[i];

    for (int mask = 0; mask < (1 << n); ++mask) {
        bool first = true;
        for (int i = 0; i < n; ++i) {
            if ((mask >> i) & 1) {
                if (!first) cout << ' ';
                first = false;
                cout << a[i + 1];
            }
        }
        cout << '\n';
    }
    return 0;
}
