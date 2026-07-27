#include <bits/stdc++.h>
using namespace std;

// 统计多重集合的不同排列数量。
// n 较小时可以直接使用组合数表，逻辑最直观。
template <class T>
long long count_distinct_permutations(vector<T> a) {
    sort(a.begin(), a.end());

    vector<int> freq;
    for (int i = 0; i < (int)a.size(); ) {
        int j = i;
        while (j < (int)a.size() && a[j] == a[i]) ++j;
        freq.push_back(j - i);
        i = j;
    }

    int n = (int)a.size();
    vector<vector<long long>> C(n + 1, vector<long long>(n + 1, 0));
    for (int i = 0; i <= n; ++i) {
        C[i][0] = C[i][i] = 1;
        for (int j = 1; j < i; ++j) {
            C[i][j] = C[i - 1][j - 1] + C[i - 1][j];
        }
    }

    long long ans = 1;
    int remaining = n;
    for (int c : freq) {
        ans *= C[remaining][c];
        remaining -= c;
    }
    return ans;
}

// 枚举多重集合的所有不同排列。
// 先排序后压缩成 value + cnt，再用 DFS 按桶取数。
template <class T, class Emit>
void enumerate_multiset_permutations(vector<T> a, const Emit& emit) {
    sort(a.begin(), a.end());

    vector<T> value;
    vector<int> cnt;
    for (const auto& x : a) {
        if (value.empty() || value.back() != x) {
            value.push_back(x);
            cnt.push_back(1);
        } else {
            ++cnt.back();
        }
    }

    vector<T> path(a.size());

    auto dfs = [&](auto&& self, int pos) -> void {
        if (pos == (int)a.size()) {
            emit(path);
            return;
        }

        for (int i = 0; i < (int)value.size(); ++i) {
            if (cnt[i] == 0) continue;
            --cnt[i];
            path[pos] = value[i];
            self(self, pos + 1);
            ++cnt[i];
        }
    };

    if (a.empty()) {
        emit(path);
        return;
    }

    dfs(dfs, 0);
}
