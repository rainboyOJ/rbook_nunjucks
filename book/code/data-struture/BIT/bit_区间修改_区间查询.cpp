#include <bits/stdc++.h>
using namespace std;

// 双树状数组：区间加、区间和
// b[i] = a[i] - a[i - 1] 是差分数组。
// bit1 维护 b[i]，bit2 维护 i * b[i]。
struct RangeFenwick {
    int n = 0;
    vector<long long> bit1, bit2;

    RangeFenwick(int n = 0) { init(n); }

    void init(int size) {
        n = size;
        bit1.assign(n + 2, 0);
        bit2.assign(n + 2, 0);
    }

    static int lowbit(int x) { return x & -x; }

    void add(vector<long long> &bit, int pos, long long value) {
        for (int i = pos; i <= n; i += lowbit(i))
            bit[i] += value;
    }

    long long sum(const vector<long long> &bit, int pos) const {
        long long answer = 0;
        for (int i = pos; i > 0; i -= lowbit(i))
            answer += bit[i];
        return answer;
    }

    // 差分数组在 l 处增加 value，在 r+1 处抵消影响。
    void range_add(int l, int r, long long value) {
        add(bit1, l, value);
        add(bit1, r + 1, -value);
        add(bit2, l, value * l);
        add(bit2, r + 1, -value * (r + 1));
    }

    // 前缀和：sum(a[1..x]) = (x+1) * sum(bit1) - sum(bit2)
    long long prefix_sum(int x) const {
        return 1LL * (x + 1) * sum(bit1, x) - sum(bit2, x);
    }

    long long range_sum(int l, int r) const {
        return prefix_sum(r) - prefix_sum(l - 1);
    }
};

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n, q;
    cin >> n >> q;

    RangeFenwick bit(n);
    for (int i = 1; i <= n; i++) {
        long long value;
        cin >> value;
        bit.range_add(i, i, value);
    }

    while (q--) {
        int op, l, r;
        cin >> op >> l >> r;
        if (op == 1) {
            long long value;
            cin >> value;
            bit.range_add(l, r, value);
        } else {
            cout << bit.range_sum(l, r) << '\n';
        }
    }

    return 0;
}
