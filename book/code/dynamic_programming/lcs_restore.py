# 最长公共子序列（路径还原）：返回 (LCS 长度, 其中一个最长公共子序列)。
# a、b 用 0 下标；回溯时 dp[i-1][j] >= dp[i][j-1] 优先上移（取等方向与 C++ 一致），故并列解也逐字相同。
# 空串（n = 0 或 m = 0）返回 (0, "")。

type Table = list[list[int]]  # dp 为 (n+1) × (m+1)，下标 0 表示空前缀


def lcs_restore(a: str, b: str) -> tuple[int, str]:
    """返回 (最长公共子序列长度, 其中一个最长公共子序列)。"""
    n = len(a)
    m = len(b)
    dp: Table = [[0] * (m + 1) for _ in range(n + 1)]

    for i in range(1, n + 1):
        for j in range(1, m + 1):
            if a[i - 1] == b[j - 1]:
                dp[i][j] = dp[i - 1][j - 1] + 1
            elif dp[i - 1][j] > dp[i][j - 1]:
                dp[i][j] = dp[i - 1][j]
            else:
                dp[i][j] = dp[i][j - 1]

    chars: list[str] = []
    i, j = n, m
    while i > 0 and j > 0:
        if a[i - 1] == b[j - 1]:
            chars.append(a[i - 1])  # 该字符必在某个 LCS 中
            i -= 1
            j -= 1
        elif dp[i - 1][j] >= dp[i][j - 1]:
            i -= 1  # 取等时向上走，与 C++ 的 >= 分支保持一致
        else:
            j -= 1
    chars.reverse()  # 回溯是逆序收集的，反转后才成正序

    return dp[n][m], "".join(chars)
