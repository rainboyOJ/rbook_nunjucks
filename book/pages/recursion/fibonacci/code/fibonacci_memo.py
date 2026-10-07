# 斐波那契（记忆化）：memo[i] 保存第 i 项，未算过为 0。
# C++ 递归深度可达 n（n ≤ 1e5），Python 默认递归限制约 1000，
# 故这里改成等价的自底向上递推，避免爆栈。结果与记忆化递归逐位相同。
# C++ 的 long long 在 n 较大时会溢出，Python int 任意精度不受影响。
import sys


def fibonacci(n: int) -> int:
    if n == 1 or n == 2:
        return 1
    memo = [0] * (n + 1)
    memo[1] = 1
    if n >= 2:
        memo[2] = 1
    for i in range(3, n + 1):
        memo[i] = memo[i - 1] + memo[i - 2]
    return memo[n]


def main() -> None:
    data = sys.stdin.buffer.read().split()
    n = int(data[0])
    # Python 3.11+ 默认限制整数转字符串最多 4300 位；n 较大时 fib(n) 位数远超此值，
    # 这里解除限制，才能像 C++ 一样把结果完整打印出来。
    if hasattr(sys, "set_int_max_str_digits"):
        sys.set_int_max_str_digits(0)
    print(fibonacci(n))


if __name__ == "__main__":
    main()
