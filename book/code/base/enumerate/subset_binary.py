# 用二进制状态枚举所有子集：mask 的第 i 位为 1 表示选择 a[i + 1]。
# 本文件提供纯函数 enumerate_subsets（C++ 的逻辑写在 main 里，这里抽成自由函数），
# 输入 1 下标数组 a（a[0] 不用），按 mask 递增顺序返回每个子集的元素列表。

type Subsets = list[list[int]]


def enumerate_subsets(n: int, a: list[int]) -> Subsets:
    # 枚举顺序固定：mask 从 0（空集）到 2^n - 1，低位 i 对应 a[i + 1]。
    res: Subsets = []
    for mask in range(1 << n):
        cur: list[int] = []
        for i in range(n):
            if (mask >> i) & 1:
                cur.append(a[i + 1])
        res.append(cur)
    return res
