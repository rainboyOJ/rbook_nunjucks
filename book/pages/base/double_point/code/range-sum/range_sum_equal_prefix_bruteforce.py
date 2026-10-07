# 前缀和排序后两两配对，输出所有和等于 target 的 1 下标闭区间。
# 前缀对按 (值, 下标) 全序排序，与 C++ 比较器一致，避免不稳定排序带来的顺序歧义。
import sys


def main() -> None:
    data = iter(map(int, sys.stdin.buffer.read().split()))
    n = next(data)
    prefix = [(0, 0)] * (n + 1)
    for i in range(1, n + 1):
        prefix[i] = (prefix[i - 1][0] + next(data), i)
    target = next(data)

    prefix.sort()

    out: list[str] = []
    for j in range(1, n + 1):
        for i in range(j):
            if prefix[j][0] - prefix[i][0] == target:
                l = min(prefix[i][1], prefix[j][1]) + 1
                r = max(prefix[i][1], prefix[j][1])
                out.append(f"{l} {r}")

    if out:
        sys.stdout.write("\n".join(out) + "\n")


if __name__ == "__main__":
    main()
