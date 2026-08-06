#include <bits/stdc++.h>
using namespace std;

// 单点 chmax、前缀最大值。
// 更新只能写成 a[pos] = max(a[pos], value)，不能任意赋值。
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
