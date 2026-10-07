# 求元素和 >= target 的最短连续子数组长度（前缀和暴力版）；数组必须全为正整数。
# a 用 1 下标存储；若无解输出 0。固定左端点后一旦满足即 break，因为右端点越远越长。
import sys


def main() -> None:
    data = iter(map(int, sys.stdin.buffer.read().split()))
    n = next(data)
    prefix = [0] * (n + 1)
    for i in range(1, n + 1):
        prefix[i] = prefix[i - 1] + next(data)
    target = next(data)

    ans = n + 1
    for left in range(1, n + 1):
        for right in range(left, n + 1):
            if prefix[right] - prefix[left - 1] >= target:
                ans = min(ans, right - left + 1)
                break

    print(0 if ans == n + 1 else ans)


if __name__ == "__main__":
    main()
