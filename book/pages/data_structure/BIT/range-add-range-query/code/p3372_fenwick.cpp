#include <bits/stdc++.h>
using namespace std;

template <typename T>
struct RangeFenwick {
    int n = 0;
    vector<T> bit_diff, bit_weighted;

    RangeFenwick(int n = 0) { init(n); }

    void init(int size) {
        n = size;
        bit_diff.assign(n + 1, 0);
        bit_weighted.assign(n + 1, 0);
    }

    static int lowbit(int x) { return x & -x; }

    void add(vector<T> &bit, int pos, T value) {
        for (int i = pos; i <= n; i += lowbit(i)) {
            bit[i] += value;
        }
    }

    T sum(const vector<T> &bit, int pos) const {
        T answer = 0;
        for (int i = pos; i > 0; i -= lowbit(i)) {
            answer += bit[i];
        }
        return answer;
    }

    void range_add(int left, int right, T value) {
        add(bit_diff, left, value);
        add(bit_diff, right + 1, -value);
        add(bit_weighted, left, value * static_cast<T>(left));
        add(bit_weighted, right + 1, -value * static_cast<T>(right + 1));
    }

    T prefix_sum(int pos) const {
        return static_cast<T>(pos + 1) * sum(bit_diff, pos)
             - sum(bit_weighted, pos);
    }

    T range_sum(int left, int right) const {
        return prefix_sum(right) - prefix_sum(left - 1);
    }
};

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n, m;
    cin >> n >> m;

    RangeFenwick<long long> bit(n);
    for (int i = 1; i <= n; ++i) {
        long long value;
        cin >> value;
        bit.range_add(i, i, value);
    }

    while (m--) {
        int operation, left, right;
        cin >> operation >> left >> right;
        if (operation == 1) {
            long long value;
            cin >> value;
            bit.range_add(left, right, value);
        } else {
            cout << bit.range_sum(left, right) << '\n';
        }
    }

    return 0;
}
