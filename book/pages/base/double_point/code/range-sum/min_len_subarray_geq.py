# 求元素和 >= target 的最短连续子数组长度；数组必须全为正整数。
# a 用 1 下标存储；若无解输出 0。正整数保证窗口和随右端点单调不减。
import sys


def main() -> None:
    data = iter(map(int, sys.stdin.buffer.read().split()))
    n = next(data)
    a = [0] * (n + 1)
    for i in range(1, n + 1):
        a[i] = next(data)
    target = next(data)

    ans = n + 1  # n + 1 是“无解”的哨兵长度
    left = 1
    cur = 0
    for right in range(1, n + 1):
        cur += a[right]
        while left <= right and cur >= target:
            ans = min(ans, right - left + 1)
            cur -= a[left]
            left += 1

    print(0 if ans == n + 1 else ans)


if __name__ == "__main__":
    main()
