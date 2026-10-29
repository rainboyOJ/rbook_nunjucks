# 区间最小值线段树（单点取 min 更新），节点与区间均为 1 下标。
# 与 C++ 版一一对应：全局列表 tr + init(n) + 独立函数。
# INF 既是叶子初值，也是查询无解时的哨兵；update_min 只让值变小。
# 递归深度是线段树高度 O(log n)，不会触及默认递归上限。

INF: int = 10**9  # 叶子初值 / 无解哨兵

tr: list[int] = []  # tr[p]：节点 p 管辖区间的最小值
n: int = 0          # 区间大小


def init(size: int) -> None:
    """重置为区间 [1, size]；换一组数据前必须先调用。"""
    global n, tr
    n = size
    tr = [0] * (n * 4 + 5)


def ls(p: int) -> int:
    return p << 1  # 左儿子 2p


def rs(p: int) -> int:
    return p << 1 | 1  # 右儿子 2p+1


def pushup(p: int) -> None:
    """上推：父区间的最小值 = 两个儿子的较小值。"""
    tr[p] = min(tr[ls(p)], tr[rs(p)])


def seg_build(p: int, l: int, r: int) -> None:
    """建树：所有叶子先置成 INF，表示这些位置还没有任何值。"""
    tr[p] = INF
    if l == r:
        return
    mid = (l + r) >> 1
    seg_build(ls(p), l, mid)
    seg_build(rs(p), mid + 1, r)


def update_min(pos: int, value: int, p: int, l: int, r: int) -> None:
    """单点取 min：把位置 pos 的值更新为 min(原值, value)。"""
    if l == r:
        tr[p] = min(tr[p], value)
        return
    mid = (l + r) >> 1
    if pos <= mid:
        update_min(pos, value, ls(p), l, mid)
    else:
        update_min(pos, value, rs(p), mid + 1, r)
    pushup(p)


def query_min(ql: int, qr: int, p: int, l: int, r: int) -> int:
    """返回 [ql, qr] 的最小值，空区间或不相交时返回 INF。"""
    if ql > qr or qr < l or r < ql:
        return INF
    if ql <= l and r <= qr:
        return tr[p]
    mid = (l + r) >> 1
    answer = query_min(ql, qr, ls(p), l, mid)
    answer = min(answer, query_min(ql, qr, rs(p), mid + 1, r))
    return answer
