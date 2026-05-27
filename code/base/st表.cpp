struct ST_table
{
    const int MAXN = 1e5 + 10;
    static const int LOGN = 20; // 2^17 > 100000，开到 20 足够了
    // f[i][j] 表示从 i 开始，长度为 2^j 的区间最小值
    int f[MAXN][LOGN];
    // Log[i] 存储 i 的以 2 为底的对数 (向下取整)
    int Log[MAXN];
    int n, m;
    auto better = [](int a, int b){ return a < b;};
    
    // 传入数组 a，长度为 m
    void init(int a[], int _m)
    {
        n = _m;
        for (int i = 1; i <= m; i++)
        {
            f[i][0] = a[i];
        }
        init();
    }

    void init()
    {
        // 1. 预处理 Log 数组
        // Log[1] = 0, Log[2]=1, Log[3]=1, Log[4]=2 ...
        Log[1] = 0;
        for (int i = 2; i <= n; i++)
        {
            Log[i] = Log[i / 2] + 1;
        }

        // 2. 初始化 ST 表的第一列 (长度为 2^0 = 1 的区间)
        // 这一步在输入时已经完成了，即 f[i][0] = input[i]

        // 3. 动态规划填表
        // j 是长度指数，必须在外层循环
        for (int j = 1; j <= LOGN - 1; j++)
        {
            // i 是起点
            // 注意边界：i + 2^j - 1 不能超过 m
            for (int i = 1; i + (1 << j) - 1 <= m; i++)
            {
                // 状态转移：左半部分 和 右半部分 取 min
                // 右半部分的起点是 i + 2^(j-1)
                f[i][j] = better(f[i][j - 1], f[i + (1 << (j - 1))][j - 1]);
            }
        }
    }

    int query(int l, int r)
    {
        int len = r - l + 1;
        int k = Log[len]; // 找到最大的 k 使得 2^k <= len

        // 覆盖左边 [l, l + 2^k - 1] 和 右边 [r - 2^k + 1, r]
        return better(f[l][k], f[r - (1 << k) + 1][k]);
    }

} st;