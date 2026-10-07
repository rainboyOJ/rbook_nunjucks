# 递归求 1 + 2 + ... + n，f(n) = n + f(n-1)，f(1) = 1。
# 递归深度为 n，n 较大时 main 中已抬高 sys.setrecursionlimit。
# n < 1 会无限递归（与 C++ 一样越界），输入保证 n >= 1。
# 结果可能超过 32 位，Python int 任意精度，无 C++ 的 int 溢出问题。
import sys


def sum_to_n(n: int) -> int:
    if n == 1:
        return 1
    return n + sum_to_n(n - 1)


def main() -> None:
    data = iter(map(int, sys.stdin.buffer.read().split()))
    n = next(data)
    sys.setrecursionlimit(max(1000, n + 100))
    print(sum_to_n(n))


if __name__ == "__main__":
    main()
