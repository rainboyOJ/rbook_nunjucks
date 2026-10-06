# 第二类斯特林数 S(n, m)：n 个互不相同的球放进 m 个非空无标号盒的方案数。
# 递推：S(n, m) = S(n - 1, m - 1) + m * S(n - 1, m)，边界 S(0, 0) = 1。
# 调用方保证 n >= 0、0 <= m <= n；m > n 时递推自然得到 0，m < 0 是 C++ 的未定义行为，
# 本文件不处理（会抛 IndexError）。C++ 用 long long 存表，n 稍大就溢出；Python int 任意精度。

type Table = list[list[int]]  # dp[i][j] 表示 S(i, j)


def stirling_second_count(n: int, m: int) -> int:
    # C++ 原版把整段递推写在 main 里（读 n, m 后打印 dp[n][m]）；
    # 这里按"模板无输入输出"的规范抽成同参数、同返回值的函数。
    dp: Table = [[0] * (m + 1) for _ in range(n + 1)]
    dp[0][0] = 1

    for i in range(1, n + 1):
        # j 只需枚举到 min(i, m)：j > i 时 S(i, j) 恒为 0。
        for j in range(1, min(i, m) + 1):
            # 第 i 个球单独成盒（j-1 个盒装前 i-1 个）或放进已有 j 个盒之一。
            dp[i][j] = dp[i - 1][j - 1] + j * dp[i - 1][j]
    return dp[n][m]
