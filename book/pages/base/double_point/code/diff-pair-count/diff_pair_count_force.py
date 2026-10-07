# 统计 i < j 且排序后差值落在 [low, high] 内的下标对数量。
# 输入顺序：n，n 个整数 a，low，high；先排序再枚举所有对。
# 差值上界可达 2e18，Python int 任意精度，无 C++ long long 溢出问题。
import sys


def main() -> None:
    data = iter(map(int, sys.stdin.buffer.read().split()))
    n = next(data)
    a = [next(data) for _ in range(n)]
    low = next(data)
    high = next(data)

    a.sort()

    ans = 0
    for i in range(n):
        for j in range(i + 1, n):
            diff = a[j] - a[i]
            if low <= diff <= high:
                ans += 1

    print(ans)


if __name__ == "__main__":
    main()
