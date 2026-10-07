# 统计 i < j 且排序后差值落在 [low, high] 内的下标对数量（双指针 O(n log n)）。
# 输入顺序：n，n 个整数 a，low，high；重复元素按下标对计数。
# 两个右指针分别维护“差值 >= low”和“差值 > high”的分界，二者之差即合法右端点个数。
import sys


def main() -> None:
    data = iter(map(int, sys.stdin.buffer.read().split()))
    n = next(data)
    a = [next(data) for _ in range(n)]
    low = next(data)
    high = next(data)

    a.sort()

    ans = 0
    first_ge_low = 1
    first_gt_high = 1
    for i in range(n):
        first_ge_low = max(first_ge_low, i + 1)
        first_gt_high = max(first_gt_high, i + 1)

        while first_ge_low < n and a[first_ge_low] - a[i] < low:
            first_ge_low += 1
        while first_gt_high < n and a[first_gt_high] - a[i] <= high:
            first_gt_high += 1

        ans += first_gt_high - first_ge_low

    print(ans)


if __name__ == "__main__":
    main()
