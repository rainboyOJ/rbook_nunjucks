# 在非递减数组 a[1..n] 中，为每个查询 x 找第一个满足 a[i] >= x 的位置。
# a 用 1 下标存储，a[0] 不用；找不到时输出 not found。
import sys


def main() -> None:
    data = iter(map(int, sys.stdin.buffer.read().split()))
    n = next(data)
    m = next(data)

    a = [0] * (n + 1)
    for i in range(1, n + 1):
        a[i] = next(data)

    out: list[str] = []
    for _ in range(m):
        x = next(data)
        pos = n + 1  # n + 1 是“未找到”的哨兵位置
        for i in range(1, n + 1):
            if a[i] >= x:
                pos = i
                break

        if pos == n + 1:
            out.append("not found")
        else:
            out.append(f"{a[pos]} {pos}")

    if out:
        sys.stdout.write("\n".join(out) + "\n")


if __name__ == "__main__":
    main()
