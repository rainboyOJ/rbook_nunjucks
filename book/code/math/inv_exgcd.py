# 扩展欧几里得：返回 (d, x, y)，满足 a * x + b * y = d = gcd(a, b)。
# C++ 用 i64& x, i64& y 出参，Python 没有引用出参，改为返回三元组，顺序为 (d, x, y)。
# 调用方保证 a, b >= 0：C++ 的 % 是向零截断取模，Python 是向下取模，负数输入下
# 两者的 (x, y) 会不同（|d| 相同）。
# C++ 的 long long 在 a * x + b * y 处可能溢出；Python int 任意精度。

type Triple = tuple[int, int, int]  # (gcd, x, y)


def exgcd(a: int, b: int) -> Triple:
    if b == 0:
        # 递归基：gcd(a, 0) = a，取 x = 1, y = 0 即可。
        return a, 1, 0

    # 由 b * x' + (a % b) * y' = d 及 a % b = a - (a // b) * b 反推：
    # a * y' + b * (x' - (a // b) * y') = d，所以新系数是 (y', x' - (a // b) * y')。
    d, x_next, y_next = exgcd(b, a % b)
    return d, y_next, x_next - (a // b) * y_next


def inverse(a: int, mod: int) -> int:
    d, x, _ = exgcd(a, mod)
    if d != 1:
        return -1  # a 与 mod 不互质时逆元不存在
    # x 可能是负数，先对 mod 取模再调整到 [0, mod)。
    return (x % mod + mod) % mod
