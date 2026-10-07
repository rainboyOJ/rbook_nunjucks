# 斐波那契（朴素递归）：不加记忆化，复杂度 O(2^n)，只适合很小的 n。
# n 较大时递归深度为 n，main 中已抬高 sys.setrecursionlimit；
# C++ 的 long long 溢出问题在 Python 中不存在（int 任意精度）。
import sys


def fibonacci(n: int) -> int:
    if n == 1 or n == 2:
        return 1
    return fibonacci(n - 1) + fibonacci(n - 2)


def main() -> None:
    data = sys.stdin.buffer.read().split()
    n = int(data[0])
    sys.setrecursionlimit(max(1000, n + 100))
    print(fibonacci(n))


if __name__ == "__main__":
    main()
