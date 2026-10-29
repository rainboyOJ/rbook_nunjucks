# 区间赋值 + 区间求和线段树（懒标记），节点与区间均为 1 下标。
# 与 C++ 版一一对应：全局列表 base / tr / lazy / tag，配 init(n) 与独立函数。
# 懒标记不变量：tag[p] 为真时，p 的整个区间被统一赋为 lazy[p]，但两个孩子的 tr
# 还没跟上；pushdown 把标记下放到孩子并清空。段内关系 tr[p] = lazy[p] * 区间长度。
# 递归深度是线段树高度 O(log n)，不会触及默认递归上限。

base: list[int] = []      # 原始数组，建树时用 base[1..n]
tr: list[int] = []        # tr[p]：节点 p 管辖区间的和
lazy: list[int] = []      # lazy[p]：待下传的赋值值
tag: list[bool] = []      # tag[p]：p 是否挂着未下传的赋值标记
n: int = 0                # 数组长度


def init(size: int) -> None:
    """重置为区间 [1, size]；换一组数据前必须先调用。"""
    global n, base, tr, lazy, tag
    n = size
    base = [0] * (n + 1)
    tr = [0] * (n * 4 + 5)
    lazy = [0] * (n * 4 + 5)
    tag = [False] * (n * 4 + 5)


def ls(p: int) -> int:
    return p << 1  # 左儿子 2p


def rs(p: int) -> int:
    return p << 1 | 1  # 右儿子 2p+1


def apply(p: int, l: int, r: int, value: int) -> None:
    """把节点 p 的整个区间 [l, r] 赋值为 value。"""
    tr[p] = value * (r - l + 1)
    lazy[p] = value
    tag[p] = True


def pushdown(p: int, l: int, r: int) -> None:
    """下推赋值标记；叶子没有孩子可下放，必须提前返回。"""
    if not tag[p] or l == r:
        return
    mid = (l + r) >> 1
    apply(ls(p), l, mid, lazy[p])
    apply(rs(p), mid + 1, r, lazy[p])
    tag[p] = False  # 标记已经下放，清掉


def seg_build(p: int, l: int, r: int) -> None:
    """用 base[1..n] 建树。"""
    tag[p] = False  # 建树时顺手清标记，换数据不必整体清零
    if l == r:
        tr[p] = base[l]
        return
    mid = (l + r) >> 1
    seg_build(ls(p), l, mid)
    seg_build(rs(p), mid + 1, r)
    tr[p] = tr[ls(p)] + tr[rs(p)]


def assign_range(ql: int, qr: int, value: int, p: int, l: int, r: int) -> None:
    """把 [ql, qr] 全部赋值为 value。"""
    if qr < l or r < ql:
        return
    if ql <= l and r <= qr:
        apply(p, l, r, value)
        return
    pushdown(p, l, r)  # 要进入孩子，先把标记下放
    mid = (l + r) >> 1
    assign_range(ql, qr, value, ls(p), l, mid)
    assign_range(ql, qr, value, rs(p), mid + 1, r)
    tr[p] = tr[ls(p)] + tr[rs(p)]


def range_sum(ql: int, qr: int, p: int, l: int, r: int) -> int:
    """返回 [ql, qr] 的区间和。"""
    if qr < l or r < ql:
        return 0
    if ql <= l and r <= qr:
        return tr[p]
    pushdown(p, l, r)  # 孩子的值可能还没跟上父节点的标记
    mid = (l + r) >> 1
    answer = range_sum(ql, qr, ls(p), l, mid)
    answer += range_sum(ql, qr, rs(p), mid + 1, r)
    return answer
