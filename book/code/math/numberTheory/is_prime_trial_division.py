# 试除法判定素数：只试除到 sqrt(n)，且跳过偶数，复杂度 O(sqrt(n))。
# 边界：n < 2 一律不是素数，n = 2 是素数。Python int 任意精度，
# 不存在 C++ 里 d * d 或 d += 2 的 long long 溢出问题。


def is_prime(n: int) -> bool:
    if n < 2:
        return False
    if n == 2:
        return True
    if n % 2 == 0:
        return False

    # d <= n // d 等价于 d * d <= n，但写成除法不会让 d * d 溢出（C++ 里正是为此）。
    d = 3
    while d <= n // d:
        if n % d == 0:
            return False
        d += 2
    return True
