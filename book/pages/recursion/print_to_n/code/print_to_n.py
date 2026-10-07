# 递归打印 1..n：前进阶段输出 dep，回溯阶段再输出 dep，结束时换行。
# 注意每行末尾带一个空格（与 C++ 逐字节一致）；n < 1 时只输出一个空行。
# 递归深度为 n + 1，main 中已抬高 sys.setrecursionlimit。
import sys

n = 0


def print_num(dep: int) -> None:
    if dep > n:
        sys.stdout.write("\n")
        return
    sys.stdout.write(f"{dep} ")  # 递归前进阶段
    print_num(dep + 1)
    sys.stdout.write(f"{dep} ")  # 递归回溯阶段


def main() -> None:
    global n
    data = sys.stdin.buffer.read().split()
    n = int(data[0])
    sys.setrecursionlimit(max(1000, n + 100))
    print_num(1)


if __name__ == "__main__":
    main()
