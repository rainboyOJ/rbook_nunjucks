# 统计满足 i < j 且 (a[i] + a[j]) % mod == 0 的数对数量。
# 对每个新元素只查它的补余数 need，保证 i < j。
# 负数取模在 C++ 里是截断取模，这里用 (x % mod + mod) % mod 统一到 [0, mod)；
# Python 的 % 本身返回非负结果，但仍保留该写法以对齐语义。
# 计数上界约 n^2/2，Python int 任意精度，无 long long 溢出问题。
import sys


def main() -> None:
    data = iter(map(int, sys.stdin.buffer.read().split()))
    n = next(data)
    mod = next(data)

    cnt = [0] * mod
    ans = 0
    for _ in range(n):
        x = next(data)
        r = (x % mod + mod) % mod
        need = (mod - r) % mod  # r == 0 时补余数仍是 0
        ans += cnt[need]
        cnt[r] += 1

    print(ans)


if __name__ == "__main__":
    main()
