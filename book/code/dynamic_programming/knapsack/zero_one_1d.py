import sys


def main():
    data = list(map(int, sys.stdin.buffer.read().split()))
    if not data:
        return

    n, C = data[0], data[1]
    dp = [0] * (C + 1)

    pos = 2
    for _ in range(n):
        w, v = data[pos], data[pos + 1]
        pos += 2

        # 01 背包必须倒序枚举容量，避免同一个物品被重复使用。
        for c in range(C, w - 1, -1):
            dp[c] = max(dp[c], dp[c - w] + v)

    print(dp[C])


if __name__ == "__main__":
    main()
