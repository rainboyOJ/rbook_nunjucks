# 统计满足 i < j 且 a[i] != a[j] 的数对数量。
# 输入约定：第一行 n，第二行 n 个取值只有 0/1 的数。
# 扫描时 cnt[1] 只累计与当前值相反的左侧元素，天然保证 i < j。
# 答案上界约 n^2/4，Python int 任意精度，不必担心 C++ 的 long long 溢出。
import sys


def main() -> None:
    data = iter(map(int, sys.stdin.buffer.read().split()))
    n = next(data)

    cnt = [0, 0]
    ans = 0
    for _ in range(n):
        x = next(data)
        ans += cnt[x ^ 1]  # 异或 1 取反：0<->1
        cnt[x] += 1

    print(ans)


if __name__ == "__main__":
    main()
