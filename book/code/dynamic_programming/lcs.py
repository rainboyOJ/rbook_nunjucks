# 最长公共子序列（LCS）长度：dp[i][j] 为 a 前 i 个字符与 b 前 j 个字符的 LCS 长度。
# a、b 用 0 下标存储；空串（n = 0 或 m = 0）结果为 0。
# C++ 用 int 存长度，最多不超过 min(n, m)，Python int 任意精度，无溢出问题。

type Table = list[list[int]]  # dp 为 (n+1) × (m+1)，下标 0 表示空前缀


def lcs(a: str, b: str) -> int:
    """返回 a 与 b 的最长公共子序列长度。"""
    n = len(a)
    m = len(b)
    dp: Table = [[0] * (m + 1) for _ in range(n + 1)]

    for i in range(1, n + 1):
        for j in range(1, m + 1):
            if a[i - 1] == b[j - 1]:
                dp[i][j] = dp[i - 1][j - 1] + 1  # 末字符相同，接在 LCS 后面
            elif dp[i - 1][j] > dp[i][j - 1]:
                dp[i][j] = dp[i - 1][j]  # 末字符不同，丢掉 a[i] 或 b[j] 中较差的一个
            else:
                dp[i][j] = dp[i][j - 1]

    return dp[n][m]
