# 汉诺塔：把 n 个盘子从 from 经 mid 移到 to，边移动边输出每一步。
# 递归深度为 n，main 中已抬高 sys.setrecursionlimit；总步数 2^n - 1 很大，
# C++ 的 long long 在 n ≥ 63 时溢出，Python int 任意精度不受影响。
import sys


def hanoi(n: int, src: str, mid: str, dst: str) -> int:
    if n == 0:
        return 0
    moves = 0
    moves += hanoi(n - 1, src, dst, mid)
    print(f"{src}->{dst}")
    moves += 1
    moves += hanoi(n - 1, mid, src, dst)
    return moves


def main() -> None:
    data = sys.stdin.buffer.read().split()
    n = int(data[0])
    sys.setrecursionlimit(max(1000, n + 100))
    print(hanoi(n, "A", "B", "C"))


if __name__ == "__main__":
    main()
