#include <bits/stdc++.h>
using namespace std;

// 递归实现 n 层循环，每层都从 [0, m) 中选择一个值。
// emit(path) 会收到一个长度为 n 的完整序列。
template <class Emit>
void enumerate_dynamic_loop(int n, int m, const Emit& emit) {
    if (n < 0 || m < 0) return;

    vector<int> path(n, 0);

    auto dfs = [&](auto&& self, int dep) -> void {
        if (dep == n) {
            emit(path);
            return;
        }

        for (int x = 0; x < m; ++x) {
            path[dep] = x;
            self(self, dep + 1);
        }
    };

    dfs(dfs, 0);
}
