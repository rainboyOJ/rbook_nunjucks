# 线性（欧拉）筛的全局状态版，与 C++ 版结构一一对应。
# 调用契约：get_primes_euler(n) 会重置 is_composite 与 primes；调用后 primes 是
# [2, n] 内升序素数表，is_composite[i]（0 下标，长度 n + 5）为 True 表示 i 是合数。
# 一行调用示例：get_primes_euler(1000)
# 调用方保证 n >= 0；C++ 里 n 为负会让 assign 收到巨大的 size_t，Python 无此问题。

MAX_N: int = 1000000  # 最大范围（C++ 常量，调用方可按需使用）

primes: list[int] = []
is_composite: list[bool] = []


def get_primes_euler(n: int) -> None:
    # C++ 是 is_composite.assign(n + 5, 0)，多留 5 个位置防止边界越界。
    is_composite.clear()
    is_composite.extend([False] * (n + 5))
    primes.clear()

    for i in range(2, n + 1):
        if not is_composite[i]:
            primes.append(i)

        for p in primes:
            # p * i > n 就停；C++ 用 1LL * p * i 防溢出，Python int 无此问题。
            if p * i > n:
                break

            is_composite[i * p] = True

            # 核心：i 被 p 整除说明 p 是 i 的最小质因子，
            # 再枚举更大的质数 p' 会得到最小质因子仍为 p 的合数 i * p'，
            # 那个合数应留给 i * p' / p 去筛，否则会重复标记。
            if i % p == 0:
                break
