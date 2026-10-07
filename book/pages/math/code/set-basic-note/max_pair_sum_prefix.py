# 求 i < j 时 a[i] + a[j] 的最大值，1 下标输入（a[1..n]）。
# 不变量：处理到 i 时 prefix_max 恰为 a[1..i-1] 的最大值。
# n < 2 时无合法配对，与 C++ 一致输出 LLONG_MIN，故用哨兵 NEG。
# 数值可能超过 64 位，Python int 任意精度，无 long long 溢出问题。
import sys

NEG = -(1 << 63)  # LLONG_MIN，对齐 C++ 初值语义


def main() -> None:
    data = iter(map(int, sys.stdin.buffer.read().split()))
    n = next(data)
    a = [0] + [next(data) for _ in range(n)]

    prefix_max = a[1]
    ans = NEG
    for i in range(2, n + 1):
        ans = max(ans, prefix_max + a[i])
        prefix_max = max(prefix_max, a[i])

    print(ans)


if __name__ == "__main__":
    main()
