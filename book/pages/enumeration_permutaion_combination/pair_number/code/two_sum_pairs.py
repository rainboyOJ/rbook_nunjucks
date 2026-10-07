# 统计满足 i < j 且 a[i] + a[j] == target 的数对数量。
# 对每个新元素只查 target - x 的历史出现次数，保证 i < j。
# 计数上界约 n^2/2，Python int 任意精度，无 long long 溢出问题。
import sys


def main() -> None:
    data = iter(map(int, sys.stdin.buffer.read().split()))
    n = next(data)
    target = next(data)

    cnt: dict[int, int] = {}
    ans = 0
    for _ in range(n):
        x = next(data)
        ans += cnt.get(target - x, 0)
        cnt[x] = cnt.get(x, 0) + 1

    print(ans)


if __name__ == "__main__":
    main()
