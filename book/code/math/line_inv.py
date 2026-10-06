# 线性求 1..n 每个数在模 mod 下的逆元，返回长度 n + 1 的数组 inv（1 下标，inv[0] 无意义）。
# 递推：inv[i] = (mod - mod / i) * inv[mod % i] % mod。
# 调用方保证 1 <= n < mod 且 mod 是质数：这样 mod % i < i <= n，inv[mod % i] 一定已经算好。
# C++ 的 long long 在 (mod - mod / i) * inv[mod % i] 处可能溢出；Python int 任意精度。

type InvTable = list[int]  # inv[i] 是 i 在模 mod 下的逆元


def linear_inverse(n: int, mod: int) -> InvTable:
    inv: InvTable = [0] * (n + 1)
    inv[1] = 1
    for i in range(2, n + 1):
        # mod / i 是整除；先算 mod % i 的逆元，再用 mod - mod / i 修正符号。
        inv[i] = (mod - mod // i) * inv[mod % i] % mod
    return inv
