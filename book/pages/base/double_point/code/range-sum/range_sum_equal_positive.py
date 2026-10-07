# 输出所有和恰好等于 target 的 1 下标闭区间；数组必须全为正整数。
# 正整数保证窗口和随右端点单调不减，和过大时收缩左端点不会漏解。
import sys


def main() -> None:
    data = iter(map(int, sys.stdin.buffer.read().split()))
    n = next(data)
    a = [0] * (n + 1)
    for i in range(1, n + 1):
        a[i] = next(data)
    target = next(data)

    out: list[str] = []
    left = 1
    cur = 0
    for right in range(1, n + 1):
        cur += a[right]
        while left <= right and cur > target:
            cur -= a[left]
            left += 1

        if cur == target:
            out.append(f"{left} {right}")

    if out:
        sys.stdout.write("\n".join(out) + "\n")


if __name__ == "__main__":
    main()
