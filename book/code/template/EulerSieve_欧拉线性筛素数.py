# 欧拉线性筛：每个合数只被它的最小质因子筛掉一次，复杂度 O(n)。
# get_primes(n) 重建 0..n 的筛表；is_prime(x) 在 x 超出已筛范围时会重筛到 x。
# Python int 任意精度，product = i * p 不必像 C++ 那样先转 long long 防溢出。

type IntList = list[int]  # 素数表：升序存放当前筛出的所有素数


class EulerSieve:
    """primes 是已筛出的素数，is_composite[x] 表示 x 是否已被判定为合数。"""

    primes: IntList
    is_composite: list[bool]

    def __init__(self) -> None:
        self.primes = []
        self.is_composite = []

    def get_primes(self, n: int) -> None:
        """重筛 [0, n]，结果写回 primes 与 is_composite。n < 2 时素数表为空。"""
        self.is_composite = [False] * (n + 1)
        self.primes = []
        for i in range(2, n + 1):
            if not self.is_composite[i]:
                self.primes.append(i)
            for p in self.primes:
                product = i * p
                if product > n:
                    break
                self.is_composite[product] = True
                # i 能被 p 整除时 p 就是 i 的最小质因子，再乘更大的素数会让
                # product 的最小质因子变成 p 而不是新素数，必须立刻退出。
                if i % p == 0:
                    break

    def get_primes_list(self) -> IntList:
        """返回内部素数表本身（C++ 返回 const 引用，语义相同，勿在外部修改）。"""
        return self.primes

    def is_prime(self, x: int) -> bool:
        if x < 2:
            return False
        if x < len(self.is_composite):
            return not self.is_composite[x]
        # 超出已筛范围：先补筛到 x，再查表（补筛会覆盖原来的素数表）。
        self.get_primes(x)
        return not self.is_composite[x]
