#include <bits/stdc++.h>
using namespace std;

template <typename T>
struct Fenwick {
    int n = 0;
    vector<T> tree;

    Fenwick(int n = 0) {
        init(n);
    }

    void init(int size) {
        n = size;
        tree.assign(n + 1, 0);
    }

    int lowbit(int x) const {
        return x & -x;
    }

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
        if (right + 1 <= n) add(right + 1, -value);
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

    Fenwick<long long> bit(n);
    for (int i = 1; i <= n; i++) {
        long long x;
        cin >> x;
        bit.range_add(i, i, x);
    }

    while (m--) {
        int op;
        cin >> op;
        if (op == 1) {
            int l, r;
            long long k;
            cin >> l >> r >> k;
            bit.range_add(l, r, k);
        } else {
            int x;
            cin >> x;
            cout << bit.point_query(x) << '\n';
        }
    }

    return 0;
}
