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

    T range_sum(int left, int right) const {
        return prefix_sum(right) - prefix_sum(left - 1);
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
        bit.add(i, x);
    }

    while (m--) {
        int op;
        cin >> op;
        if (op == 1) {
            int x;
            long long k;
            cin >> x >> k;
            bit.add(x, k);
        } else {
            int l, r;
            cin >> l >> r;
            cout << bit.range_sum(l, r) << '\n';
        }
    }

    return 0;
}
