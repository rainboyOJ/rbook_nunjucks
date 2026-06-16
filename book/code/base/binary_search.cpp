#include <bits/stdc++.h>
using namespace std;

// 在 [l, r] 中查找第一个满足 check(pos) 的位置。
// 要求 check 单调：false false ... false true true ... true。
// 调用时要保证 r 是一个真实或虚拟的可行位置。
template <typename Check>
int first_true(int l, int r, Check check) {
    while (l < r) {
        int mid = l + (r - l) / 2;
        if (check(mid)) r = mid;
        else l = mid + 1;
    }
    return l;
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n, m;
    cin >> n >> m;

    vector<int> a(n + 2);
    for (int i = 1; i <= n; ++i) cin >> a[i];

    // 哨兵位置 n+1：表示不存在 >= x 的元素。
    a[n + 1] = INT_MAX;

    while (m--) {
        int x;
        cin >> x;

        int pos = first_true(1, n + 1, [&](int i) {
            return a[i] >= x;
        });

        if (pos == n + 1) cout << "not found\n";
        else cout << a[pos] << ' ' << pos << '\n';
    }

    return 0;
}
