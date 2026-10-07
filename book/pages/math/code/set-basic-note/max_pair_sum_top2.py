# 求数组中两数之和的最大值：一定来自最大值与次大值。
# 一次扫描维护 first（最大）与 second（次大）；x > first 时旧 first 降级为 second。
# 初值用 LLONG_MIN 对齐 C++；n < 2 时 C++ 会溢出（有符号 UB），Python 得到
# -2^64，两者不可比，故对拍只用 n >= 2。
# 和可能超过 64 位，Python int 任意精度，无 long long 溢出问题。
import sys

NEG = -(1 << 63)  # LLONG_MIN，对齐 C++ 初值语义


def main() -> None:
    data = iter(map(int, sys.stdin.buffer.read().split()))
    n = next(data)

    first = NEG
    second = NEG
    for _ in range(n):
        x = next(data)
        if x > first:
            second = first
            first = x
        elif x > second:
            second = x

    print(first + second)


if __name__ == "__main__":
    main()
