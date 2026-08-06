#include <bits/stdc++.h>
using namespace std;

// ST 表：预处理所有长度为 2^k 的区间最值，O(1) 查询静态 RMQ
// st[i][k] = 从 i 开始、长度为 2^k 的区间最大值
// 查询时用两个可重叠的 2^k 区间覆盖 [l, r]（重叠不影响 max/min/gcd 等幂等运算）
struct SparseTable {
    int n = 0;
    vector<int> lg;               // lg[x] = floor(log2(x))，查一次取整
    vector<vector<int>> st;       // st[i][k] = max( a[i..i+2^k-1] )

    void build(const vector<int> &a) {
        n = (int)a.size() - 1;    // a 使用 1-indexed

        // 预处理 log2 表，避免每次查询重复计算
        lg.assign(n + 1, 0);
        for (int i = 2; i <= n; i++)
            lg[i] = lg[i / 2] + 1;

        int max_log = lg[n] + 1;  // 最大 k = floor(log2(n))
        st.assign(n + 1, vector<int>(max_log, 0));

        // k=0：长度为 1 的区间就是自己
        for (int i = 1; i <= n; i++)
            st[i][0] = a[i];

        // k>0：长度为 2^k 的区间 = 左 2^{k-1} + 右 2^{k-1}
        for (int k = 1; k < max_log; k++) {
            int len = 1 << k;          // 当前区间长度 2^k
            int half = len >> 1;       // 半长 2^{k-1}
            for (int i = 1; i + len - 1 <= n; i++)
                st[i][k] = max(st[i][k - 1], st[i + half][k - 1]);
        }
    }

    // 查询 [l, r] 最大值：取 k = floor(log2(r-l+1))，两部分重叠覆盖
    int query(int l, int r) const {
        int k = lg[r - l + 1];         // 最大不超过区间长的 2^k 的 k
        return max(st[l][k], st[r - (1 << k) + 1][k]);
    }
};

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n, q;
    cin >> n >> q;

    vector<int> a(n + 1);
    for (int i = 1; i <= n; i++)
        cin >> a[i];

    SparseTable table;
    table.build(a);

    while (q--) {
        int l, r;
        cin >> l >> r;
        cout << table.query(l, r) << "\n";
    }

    return 0;
}
