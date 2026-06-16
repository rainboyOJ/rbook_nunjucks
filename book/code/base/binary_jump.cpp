#include <bits/stdc++.h>
using namespace std;

// 在 [start, n] 中找最后一个满足 check(pos) 的位置。
// 要求 check 具有单调性：true ... true false ... false。
template <typename Check>
int binary_jump_last_true(int start, int n, Check check) {
    int pos = start;
    int max_step = 1;
    while ((max_step << 1) <= n) max_step <<= 1;

    for (int step = max_step; step > 0; step >>= 1) {
        int nxt = pos + step;
        if (nxt <= n && check(nxt)) {
            pos = nxt;
        }
    }
    return pos;
}

int main() {
    int n, limit;
    cin >> n >> limit;

    // 示例：找最后一个 <= limit 的位置。
    auto check = [&](int pos) {
        return pos <= limit;
    };

    cout << binary_jump_last_true(0, n, check) << "\n";
    return 0;
}
