# 质因数分解：返回 [(p1, c1), (p2, c2), ...]，p 升序且 n = p1^c1 * p2^c2 ...。
# 试除到 sqrt(当前 n)，复杂度 O(sqrt(n))。边界：n < 2 返回空列表。
# C++ 版把结果直接 printf，这里按模板规范改为返回列表，函数名与参数保持不变。


type Factors = list[tuple[int, int]]  # (质因子, 指数)


def get_prime_factors(n: int) -> Factors:
    factors: Factors = []

    # i * i <= n 里的 n 是不断除小后的 n，所以只需试到 sqrt(剩余 n)。
    i = 2
    while i * i <= n:
        if n % i == 0:
            cnt = 0
            # 把当前质因子除尽，保证之后遇到的 i 一定是质数。
            while n % i == 0:
                cnt += 1
                n //= i
            factors.append((i, cnt))
        i += 1

    # 剩下 n > 1 说明它本身是一个大于 sqrt(原始 n) 的质数。
    if n > 1:
        factors.append((n, 1))

    return factors
