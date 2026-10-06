# 三路快速排序：划分成 < key / == key / > key 三段，等于段不再递归。
# 与 C++ 版相同，用模块级全局状态，调用前按下述顺序布置（a 为 1 下标，a[0] 不用）：
#   qs3.a = [0] + a
#   qs3.quick_sort(1, n)
# 深度期望 O(log n)：随机化防止有序输入退化，但最坏（随机数不巧）仍可能到 n，
# n 逼近 1e6（C++ maxn 量级）时需要 sys.setrecursionlimit(3 * 10 ** 6) 或改迭代。
# 运行输出打印排序结果与 C++ diff 时需设置固定随机种子（见对拍驱动），模板本身无 I/O。

import random

# 模块级随机源：与 C++ 的 rand()/srand(time(0)) 对应；
# 对拍时由驱动注入固定种子保证两边选到同一基准（C++ 驱动也用同一种子调用 srand）。
_rng = random.Random()

n: int = 0
a: list[int] = []


def quick_sort(l: int, r: int) -> None:
    if l >= r:
        return

    # 1. 随机选一个基准数（防止被针对卡成 O(N^2)）。
    # randrange(len) 与 rand() % len 语义一致（都取 [0, len) 的均匀整数）。
    rand_idx = l + _rng.randrange(r - l + 1)
    a[l], a[rand_idx] = a[rand_idx], a[l]
    key = a[l]

    # 2. 定义指针：lt 指向"等于区"第一个位置，gt 指向"等于区"最后一个位置，
    # i 从 l+1 开始扫描（a[l] 本身就是 key）。
    lt = l
    gt = r
    i = l + 1

    # 3. 扫描并分类。
    while i <= gt:
        if a[i] < key:
            # 情况A：扔到左边，lt 和 i 都右移。
            a[i], a[lt] = a[lt], a[i]
            lt += 1
            i += 1
        elif a[i] > key:
            # 情况B：扔到右边，gt 左移；i 不能动，换回来的数还没检查过。
            a[i], a[gt] = a[gt], a[i]
            gt -= 1
        else:
            # 情况C：等于 key，i 右移。
            i += 1

    # 此时 [l, lt-1] < key，[lt, gt] == key（已就位，不递归），[gt+1, r] > key。
    quick_sort(l, lt - 1)
    quick_sort(gt + 1, r)
