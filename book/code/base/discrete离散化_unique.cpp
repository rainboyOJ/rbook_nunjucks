#include <bits/stdc++.h>
using namespace std;

struct Discrete {
    vector<int> xs;

    void clear() {
        xs.clear();
    }

    void add(int x) {
        xs.push_back(x);
    }

    void build() {
        sort(xs.begin(), xs.end());
        xs.erase(unique(xs.begin(), xs.end()), xs.end());
    }

    // 返回 x 离散化后的 1 下标编号。
    int get(int x) const {
        return lower_bound(xs.begin(), xs.end(), x) - xs.begin() + 1;
    }

    // 找不到时返回 -1，适合查询不确定是否出现过的值。
    int get_maybe(int x) const {
        auto it = lower_bound(xs.begin(), xs.end(), x);
        if (it == xs.end() || *it != x) return -1;
        return it - xs.begin() + 1;
    }

    // 根据 1 下标编号找回原值。
    int origin(int k) const {
        return xs[k - 1];
    }

    int size() const {
        return (int)xs.size();
    }
};

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n;
    cin >> n;

    vector<int> a(n);
    Discrete disc;

    for (int i = 0; i < n; i++) {
        cin >> a[i];
        disc.add(a[i]);
    }

    disc.build();

    cout << disc.size() << '\n';
    for (int i = 0; i < n; i++) {
        if (i > 0) cout << ' ';
        cout << disc.get(a[i]);
    }
    cout << '\n';

    return 0;
}
