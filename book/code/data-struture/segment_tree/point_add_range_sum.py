# 单点加 + 区间求和线段树，节点与区间均为 1 下标。
# 与 C++ 版一一对应：全局列表 base / tr，配 init(n) 与独立函数。
# 调用顺序：init(n) -> 填 base[1..n] -> seg_build(1, 1, n) -> point_add / range_sum。
# 递归深度是线段树高度 O(log n)，不会触及默认递归上限。
# C++ 用 long long 存区间和防溢出；Python int 任意精度，该坑不存在。

base: list[int] = []  # 原始数组，建树时用 base[1..n]
tr: list[int] = []    # tr[p]：节点 p 管辖区间的和
n: int = 0            # 数组长度


def init(size: int) -> None:
    """重置为区间 [1, size]；换一组数据前必须先调用。"""
    global n, base, tr
    n = size
    base = [0] * (n + 1)
    tr = [0] * (n * 4 + 5)


def ls(p: int) -> int:
    return p << 1  # 左儿子 2p


def rs(p: int) -> int:
    return p << 1 | 1  # 右儿子 2p+1


def pushup(p: int) -> None:
    """上推：父区间的和 = 两个儿子的和。"""
    tr[p] = tr[ls(p)] + tr[rs(p)]


def seg_build(p: int, l: int, r: int) -> None:
    """用 base[1..n] 建树。"""
    if l == r:
        tr[p] = base[l]
        return
    mid = (l + r) >> 1
    seg_build(ls(p), l, mid)
    seg_build(rs(p), mid + 1, r)
    pushup(p)


def point_add(pos: int, value: int, p: int, l: int, r: int) -> None:
    """单点加：把位置 pos 的值加上 value。"""
    if l == r:
        tr[p] += value
        return
    mid = (l + r) >> 1
    if pos <= mid:
        point_add(pos, value, ls(p), l, mid)
    else:
        point_add(pos, value, rs(p), mid + 1, r)
    pushup(p)


def range_sum(ql: int, qr: int, p: int, l: int, r: int) -> int:
    """返回 [ql, qr] 的区间和。"""
    if qr < l or r < ql:
        return 0  # 完全不相交
    if ql <= l and r <= qr:
        return tr[p]  # 完全被覆盖
    mid = (l + r) >> 1
    answer = range_sum(ql, qr, ls(p), l, mid)
    answer += range_sum(ql, qr, rs(p), mid + 1, r)
    return answer
