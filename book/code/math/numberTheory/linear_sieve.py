# 线性（欧拉）筛：每个合数只被它的最小质因子筛掉一次，复杂度 O(n)。
# build(n) 后可读 primes（升序素数表）、min_factor[i]（i 的最小质因子，i < 2 为 0）、
# is_composite[i]（i 是否为合数）；is_prime(x) 要求先 build 且 0 <= x <= n。
# min_factor / is_composite 下标为 0..n，长度都是 n + 1。


class LinearSieve:
    """用 i 的最小质因子 p 生成 i * p，保证每个合数恰好被标记一次。"""

    primes: list[int]
    min_factor: list[int]
    is_composite: list[bool]

    def __init__(self) -> None:
        # 对应 C++ 默认构造出的三个空 vector，方便先建对象再 build。
        self.primes = []
        self.min_factor = []
        self.is_composite = []

    def build(self, n: int) -> None:
        self.primes = []
        self.min_factor = [0] * (n + 1)
        self.is_composite = [False] * (n + 1)

        for i in range(2, n + 1):
            if not self.is_composite[i]:
                self.primes.append(i)
                self.min_factor[i] = i

            for p in self.primes:
                # p > n // i 时 i * p 已超过 n，后续质数只会更大，直接停。
                if p > n // i:
                    break
                x = i * p
                self.is_composite[x] = True
                self.min_factor[x] = p

                # p 是 i 的最小质因子时，不能再用更大的质数 p' 生成 i * p'：
                # 那些数的最小质因子仍是 p，应留给 i * p' / p 去筛，否则重复标记。
                if i % p == 0:
                    break

    def is_prime(self, x: int) -> bool:
        # 调用方需保证先 build 且 x <= n；x < 2 时短路，不会访问 is_composite。
        return x >= 2 and not self.is_composite[x]
