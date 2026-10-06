# 欧拉函数 φ(n)：1..n 中与 n 互质的数的个数，先试除分解质因数，再按
# φ(n) = n * Π (p - 1) / p 累乘（每步先除后乘，避免中间结果变大）。
# 边界：n = 1 返回 1；n = 0 返回 0；n < 0 时直接返回 n，与 C++ 版一致（调用方保证 n >= 1）。
# C++ 的 p * p <= n 在 p 接近 long long 上限时会溢出；Python int 任意精度。


def euler_phi(n: int) -> int:
    ans = n

    p = 2
    # 注意 n 在循环里会被不断除掉已找到的质因子，所以 p * p <= n 用的是缩小后的 n。
    while p * p <= n:
        if n % p != 0:
            p += 1
            continue

        ans = ans // p * (p - 1)
        while n % p == 0:
            n //= p
        p += 1

    # 循环结束后剩下的 n > 1 说明它本身是一个质因子。
    if n > 1:
        ans = ans // n * (n - 1)
    return ans
