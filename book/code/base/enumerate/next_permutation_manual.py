# 手写字典序 next_permutation：原地修改 a，产生严格意义上"下一个更大"的排列。
# 返回 False 表示 a 已是最后一个排列，此时 a 保持不变（标准库版会反转成最小排列，注意区别）。


def next_permutation_manual(a: list[int]) -> bool:
    n = len(a)

    # 1. 找最右边的 i 满足 a[i] < a[i + 1]。
    # 此时后缀 a[i + 1..n - 1] 非递增。
    i = n - 2
    while i >= 0 and a[i] >= a[i + 1]:
        i -= 1
    if i < 0:
        return False

    # 2. 找最右边的、比 a[i] 大的元素。
    j = n - 1
    while a[j] <= a[i]:
        j -= 1

    # 3. 让排列稍微变大，再把后缀最小化。
    a[i], a[j] = a[j], a[i]
    a[i + 1:] = reversed(a[i + 1:])
    return True
