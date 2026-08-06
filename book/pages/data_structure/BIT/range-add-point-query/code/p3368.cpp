#include <bits/stdc++.h>
using namespace std;

template <typename T>
struct RangeAddPointQueryFenwick {
    int n = 0;
    vector<T> tree;

    RangeAddPointQueryFenwick(int n = 0) { init(n); }

    void init(int size) {
        n = size;
        tree.assign(n + 1, 0);
    }

    static int lowbit(int x) { return x & -x; }

    void add(int pos, T value) {
        for (int i = pos; i <= n; i += lowbit(i)) {
            tree[i] += value;
        }
    }

    T prefix_sum(int pos) const {
        T answer = 0;
        for (int i = pos; i > 0; i -= lowbit(i)) {
            answer += tree[i];
        }
        return answer;
    }

    void range_add(int left, int right, T value) {
        add(left, value);
        if (right < n) add(right + 1, -value);
    }

    T point_query(int pos) const {
        return prefix_sum(pos);
    }
};

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n, m;
    cin >> n >> m;

    RangeAddPointQueryFenwick<long long> bit(n);
    long long previous = 0;
    for (int i = 1; i <= n; ++i) {
        long long value;
        cin >> value;
        bit.add(i, value - previous);
        previous = value;
    }

    while (m--) {
        int operation;
        cin >> operation;
        if (operation == 1) {
            int left, right;
            long long value;
            cin >> left >> right >> value;
            bit.range_add(left, right, value);
        } else {
            int pos;
            cin >> pos;
            cout << bit.point_query(pos) << '\n';
        }
    }

    return 0;
}
