#include <bits/stdc++.h>
using namespace std;

template <typename T>
struct FenwickPrefixMax {
    int n = 0;
    T identity = numeric_limits<T>::lowest();
    vector<T> tree;

    FenwickPrefixMax(int n = 0) { init(n); }

    void init(int size) {
        n = size;
        tree.assign(n + 1, identity);
    }

    static int lowbit(int x) { return x & -x; }

    void chmax(int pos, T value) {
        for (int i = pos; i <= n; i += lowbit(i)) {
            tree[i] = max(tree[i], value);
        }
    }

    T prefix_max(int pos) const {
        T answer = identity;
        for (int i = pos; i > 0; i -= lowbit(i)) {
            answer = max(answer, tree[i]);
        }
        return answer;
    }
};

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n, m;
    cin >> n >> m;

    FenwickPrefixMax<long long> bit(n);
    for (int i = 1; i <= n; ++i) {
        long long value;
        cin >> value;
        bit.chmax(i, value);
    }

    while (m--) {
        int operation;
        cin >> operation;
        if (operation == 1) {
            int pos;
            long long value;
            cin >> pos >> value;
            bit.chmax(pos, value);
        } else {
            int right;
            cin >> right;
            cout << bit.prefix_max(right) << '\n';
        }
    }

    return 0;
}
