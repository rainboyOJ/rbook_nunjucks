# 统计满足 i < j、a[i] == a[j] 且 j - i <= k 的数对数量。
# 不变量：读入 a[j] 前，cnt 里恰好是窗口 [j-k, j-1] 内各值的出现次数。
# 下标从 0 开始；先剔除 a[j-k-1] 再加入 a[j]，保证距离恰好不超过 k。
# 计数上界约 n^2/2，Python int 任意精度，不会像 C++ 的 long long 溢出。
import sys

prev: list[int] = []  # prev[j] 保存已读入的 a[j]，用于窗口过期时按值删除。


def main() -> None:
    data = iter(map(int, sys.stdin.buffer.read().split()))
    n = next(data)
    k = next(data)

    cnt: dict[int, int] = {}
    ans = 0
    for j in range(n):
        x = next(data)

        if j > k:
            expired = prev[j - k - 1]
            cnt[expired] -= 1
            if cnt[expired] == 0:
                del cnt[expired]

        ans += cnt.get(x, 0)
        cnt[x] = cnt.get(x, 0) + 1
        prev.append(x)

    print(ans)


if __name__ == "__main__":
    main()
