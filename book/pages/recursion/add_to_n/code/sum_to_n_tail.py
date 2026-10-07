# 尾递归求 1 + 2 + ... + n：calc(a, s) 表示已累加到 a-1、当前和为 s。
# 终止条件 a == n + 1 返回 s，对应 C++ 的 calc(1, n, 0)。
# 递归深度为 n + 1，n 较大时 main 中已抬高 sys.setrecursionlimit。
# Python 不做尾调用优化，深度仍是 O(n)；结果任意精度，无 int 溢出问题。
import sys


def calc(a: int, n: int, s: int) -> int:
    if a == n + 1:
        return s
    return calc(a + 1, n, s + a)


def main() -> None:
    data = iter(map(int, sys.stdin.buffer.read().split()))
    n = next(data)
    sys.setrecursionlimit(max(1000, n + 100))
    print(calc(1, n, 0))


if __name__ == "__main__":
    main()
