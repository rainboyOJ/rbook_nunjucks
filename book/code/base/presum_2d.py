import sys
from itertools import accumulate


input = sys.stdin.buffer.readline


def build_prefix(n, m):
    prefix = [[0] * (m + 1)]

    for _ in range(n):
        row_prefix = accumulate(map(int, input().split()), initial=0)
        prefix.append([up + left for up, left in zip(prefix[-1], row_prefix)])

    return prefix


def rect_sum(prefix, x1, y1, x2, y2):
    return (
        prefix[x2][y2]
        - prefix[x1 - 1][y2]
        - prefix[x2][y1 - 1]
        + prefix[x1 - 1][y1 - 1]
    )


def main():
    first = input().split()
    if not first:
        return

    n, m, q = map(int, first)
    prefix = build_prefix(n, m)

    answer = []
    for _ in range(q):
        x1, y1, x2, y2 = map(int, input().split())
        answer.append(str(rect_sum(prefix, x1, y1, x2, y2)))

    sys.stdout.write("\n".join(answer))


if __name__ == "__main__":
    main()
