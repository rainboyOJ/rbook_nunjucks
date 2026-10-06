# 快速幂：计算 base^exp mod mod，要求 mod > 0，O(log exp) 次乘法。
# 边界：exp <= 0 时循环不执行，返回 1 % mod（与 C++ 一致）。
# C++ 的 ans * base 可能溢出 long long；Python int 任意精度，无需 __int128。


def quick_pow(base: int, exp: int, mod: int) -> int:
    # 1 % mod：mod = 1 时结果为 0，保证返回值落在 [0, mod)。
    ans = 1 % mod
    base %= mod

    while exp > 0:
        # exp 二进制最低位为 1，就把当前的 base^(2^k) 乘进答案。
        if exp & 1:
            ans = ans * base % mod
        base = base * base % mod
        # 右移一位，处理下一个二进制位。
        exp >>= 1

    return ans
