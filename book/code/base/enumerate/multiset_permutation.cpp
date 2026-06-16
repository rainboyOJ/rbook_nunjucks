#include <bits/stdc++.h>
using namespace std;

// Enumerate all distinct permutations of a multiset in lexicographic order.
// Input:
//   n
//   a1 a2 ... an
// Output:
//   first line: number of distinct permutations
//   following lines: each distinct permutation

int n;
vector<int> value_list;
vector<int> cnt;
vector<int> path;
vector<vector<long long>> C;

long long count_permutations() {
    C.assign(n + 1, vector<long long>(n + 1, 0));
    for (int i = 0; i <= n; ++i) {
        C[i][0] = C[i][i] = 1;
        for (int j = 1; j < i; ++j) {
            C[i][j] = C[i - 1][j - 1] + C[i - 1][j];
        }
    }

    long long ans = 1;
    int remaining = n;
    for (int c : cnt) {
        ans *= C[remaining][c];
        remaining -= c;
    }
    return ans;
}

void dfs(int pos) {
    if (pos == n) {
        for (int i = 0; i < n; ++i) {
            if (i) cout << ' ';
            cout << path[i];
        }
        cout << '\n';
        return;
    }

    for (int i = 0; i < (int)value_list.size(); ++i) {
        if (cnt[i] == 0) continue;
        --cnt[i];
        path[pos] = value_list[i];
        dfs(pos + 1);
        ++cnt[i];
    }
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    cin >> n;
    vector<int> a(n);
    for (int &x : a) cin >> x;

    sort(a.begin(), a.end());
    for (int x : a) {
        if (value_list.empty() || value_list.back() != x) {
            value_list.push_back(x);
            cnt.push_back(1);
        } else {
            ++cnt.back();
        }
    }

    path.assign(n, 0);
    cout << count_permutations() << '\n';
    dfs(0);
    return 0;
}
