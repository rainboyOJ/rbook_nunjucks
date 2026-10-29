# 懒标记线段树（区间加 + 区间和，取模），节点与区间均为 1 下标。
# 与 C++ 版、HLD 模板里的 sgt 段一一对应：全局列表 base / tr / lazy + init(n)。
# 懒标记不变量：lazy[p] 表示“p 的整个区间还要给每个孩子各加这么多”，
# 但不影响 p 自己（p 的 tr 已经加过了）；pushdown 后清成 0。
# 递归深度是线段树高度 O(log n)，不会触及默认递归上限。
# Python int 任意精度，不必像 C++ 那样担心 value * 长度 溢出。

base: list[int] = []  # 原始数组，建树时用 base[1..n]
tr: list[int] = []    # tr[p]：节点 p 管辖区间的和（已取模）
lazy: list[int] = []  # lazy[p]：待下传到两个孩子的加法标记
n: int = 0            # 数组长度
mod: int = 1000000007  # 取模的模数，做题时按题面赋值


def init(size: int, modulus: int) -> None:
    """重置为区间 [1, size] 与模数 modulus；换一组数据前必须先调用。"""
    global n, mod, base, tr, lazy
    n = size
    mod = modulus
    base = [0] * (n + 1)
    tr = [0] * (n * 4 + 5)
    lazy = [0] * (n * 4 + 5)


def ls(p: int) -> int:
    return p << 1  # 左儿子 2p


def rs(p: int) -> int:
    return p << 1 | 1  # 右儿子 2p+1


def norm(x: int) -> int:
    """把可能为负的 x 归一到 [0, mod)。"""
    return x % mod


def apply(p: int, l: int, r: int, value: int) -> None:
    """把节点 p 的整个区间 [l, r] 整体加 value。"""
    value = norm(value)
    tr[p] = (tr[p] + value * (r - l + 1)) % mod
    lazy[p] = (lazy[p] + value) % mod


def pushdown(p: int, l: int, r: int) -> None:
    """下推加法标记；叶子没有孩子可下放，必须提前返回。"""
    if lazy[p] == 0 or l == r:
        return
    mid = (l + r) >> 1
    apply(ls(p), l, mid, lazy[p])
    apply(rs(p), mid + 1, r, lazy[p])
    lazy[p] = 0


def seg_build(p: int, l: int, r: int) -> None:
    """用 base[1..n] 建树。"""
    lazy[p] = 0  # 建树时顺手清懒标记，换数据不必整体清零
    if l == r:
        tr[p] = norm(base[l])
        return
    mid = (l + r) >> 1
    seg_build(ls(p), l, mid)
    seg_build(rs(p), mid + 1, r)
    tr[p] = (tr[ls(p)] + tr[rs(p)]) % mod


def range_add(ql: int, qr: int, value: int, p: int, l: int, r: int) -> None:
    """把 [ql, qr] 整体加 value。"""
    if qr < l or r < ql:
        return
    if ql <= l and r <= qr:
        apply(p, l, r, value)
        return
    pushdown(p, l, r)
    mid = (l + r) >> 1
    range_add(ql, qr, value, ls(p), l, mid)
    range_add(ql, qr, value, rs(p), mid + 1, r)
    tr[p] = (tr[ls(p)] + tr[rs(p)]) % mod


def range_sum(ql: int, qr: int, p: int, l: int, r: int) -> int:
    """返回 [ql, qr] 的区间和（已取模）。"""
    if qr < l or r < ql:
        return 0
    if ql <= l and r <= qr:
        return tr[p]
    pushdown(p, l, r)
    mid = (l + r) >> 1
    answer = range_sum(ql, qr, ls(p), l, mid)
    answer += range_sum(ql, qr, rs(p), mid + 1, r)
    return answer % mod
