# 传统二路快速排序：指针错车划分，左边 <= mid、右边 >= mid。
# 与 C++ 版相同，用模块级全局状态，调用前按下述顺序布置（a 为 1 下标，a[0] 不用）：
#   qs.a = [0] + a
#   qs.quick_sort(1, n)
# 深度期望 O(log n)：取中间数做基准防有序输入退化，但最坏仍可能到 n，
# n 逼近 1e6（C++ maxn 量级）时需要 sys.setrecursionlimit(3 * 10 ** 6) 或改迭代。
# 扫描条件必须 < mid / > mid（不能 <= / >=）：让等于 mid 的元素均匀分到两边，避免树倾斜。

n: int = 0
a: list[int] = []


def quick_sort(l: int, r: int) -> None:
    # 递归出口：区间只剩一个数或没有数。
    if l >= r:
        return

    # 建议取中间的数，防止在原本有序的数组上退化成 O(N^2)。
    mid = a[(l + r) // 2]
    i = l
    j = r

    # Partition：让 [l, j] 全部 <= mid，[i, r] 全部 >= mid。
    while i <= j:
        # 左指针右移找 >= mid；等于 mid 也要停下。
        while a[i] < mid:
            i += 1
        # 右指针左移找 <= mid；等于 mid 也要停下。
        while a[j] > mid:
            j -= 1

        # 未交错则交换一对"放错位置"的数，然后双指针各进一步。
        if i <= j:
            a[i], a[j] = a[j], a[i]
            i += 1
            j -= 1

    # 此时已"错车"：j < i，分割点为 j 和 i。
    if l < j:
        quick_sort(l, j)
    if i < r:
        quick_sort(i, r)
