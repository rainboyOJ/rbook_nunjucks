#include <bits/stdc++.h>
using namespace std;

// 枚举所有 m 组合。
// a 会按输入顺序被选择；如果希望字典序输出，调用前先对 a 排序。
template <class T, class Emit>
void enumerate_combinations(const vector<T>& a, int m, const Emit& emit) {
    const int n = (int)a.size();
    if (m < 0 || m > n) return;

    vector<T> path;
    path.reserve(m);

    auto dfs = [&](auto&& self, int last) -> void {
        if ((int)path.size() == m) {
            emit(path);
            return;
        }

        int need = m - (int)path.size();
        for (int i = last + 1; i <= n - need; ++i) {
            path.push_back(a[i]);
            self(self, i);
            path.pop_back();
        }
    };

    dfs(dfs, -1);
}
