# 埃氏筛：返回 [2, n] 内所有质数，升序。
# 边界：n < 2 时返回空列表；空间 O(n)，n 很大时注意内存。
# 每个合数都被它最小的质因子筛掉一次，i 只需筛到 sqrt(n)（用 i > n // i 判断，
# 对应 C++ 防止 i * i 溢出的写法）。


def eratosthenes(n: int) -> list[int]:
    is_composite = [False] * (n + 1)
    primes: list[int] = []

    for i in range(2, n + 1):
        if is_composite[i]:
            continue

        primes.append(i)
        # i * i > n 时后面没有可筛的了；先判再进内层循环。
        if i > n // i:
            continue

        # 小于 i * i 的 i 的倍数已被更小的质因子筛过，从 i * i 开始即可。
        for j in range(i * i, n + 1, i):
            is_composite[j] = True

    return primes
