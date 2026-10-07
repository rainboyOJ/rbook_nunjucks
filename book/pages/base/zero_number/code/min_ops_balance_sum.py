# 每次操作把一个 +1 与一个 -1 配对抵消，最少操作次数 = max(正数总量, 负数绝对值总量)。
# 输入：n，随后 n 个整数；0 归入 else 分支（负数绝对值侧），与 C++ 版一致。
# 正负总量可能超出 64 位，Python int 任意精度，无 C++ long long 溢出问题。
import sys


def main() -> None:
    data = iter(map(int, sys.stdin.buffer.read().split()))
    n = next(data)

    positive_sum = 0
    negative_abs_sum = 0
    for _ in range(n):
        x = next(data)
        if x > 0:
            positive_sum += x
        else:
            negative_abs_sum += -x

    print(max(positive_sum, negative_abs_sum))


if __name__ == "__main__":
    main()
