# 最长严格上升子序列 O(n^2)：dp[i] = 以 a[i] 结尾的最长严格上升子序列长度。
# 输入：第一行 n（n 可为 0）；第二行起共 n 个整数，可跨行、可为负。
# 输出：最长严格上升子序列长度；n = 0 时输出 0。相等元素不能相接（严格上升）。
import sys


def main() -> None:
    data = iter(map(int, sys.stdin.buffer.read().split()))
    n = next(data)

    a = [0] * (n + 1)
    for i in range(1, n + 1):
        a[i] = next(data)

    dp = [1] * (n + 1)  # 每个元素自身就构成长度 1 的子序列
    ans = 0
    for i in range(1, n + 1):
        ai = a[i]
        for j in range(1, i):
            # 严格上升要求 a[j] < a[i]；a[j] == a[i] 时不能接在后面
            if a[j] < ai and dp[j] + 1 > dp[i]:
                dp[i] = dp[j] + 1
        if dp[i] > ans:
            ans = dp[i]

    print(ans)


if __name__ == "__main__":
    main()
