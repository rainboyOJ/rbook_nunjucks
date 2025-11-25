// 竞赛用简洁模板
struct ST {
    static const int MAXN = 100005;
    static const int LOGN = 17;
    int st[MAXN][LOGN];
    int log_table[MAXN];
    int n;

    void init(vector<int>& arr) {
        n = arr.size();
        // 预处理对数表
        log_table[1] = 0;
        for (int i = 2; i <= n; i++) {
            log_table[i] = log_table[i / 2] + 1;
        }

        // 初始化长度为 2^0 = 1 的区间
        for (int i = 0; i < n; i++) {
            st[i][0] = arr[i];
        }

        // 递推构建
        for (int j = 1; (1 << j) <= n; j++) {
            for (int i = 0; i + (1 << j) <= n; i++) {
                st[i][j] = max(st[i][j - 1], st[i + (1 << (j - 1))][j - 1]);
            }
        }
    }

    // 查询区间 [l, r] 的最大值 (0-indexed)
    int query(int l, int r) {
        int k = log_table[r - l + 1];
        return max(st[l][k], st[r - (1 << k) + 1][k]);
    }
};