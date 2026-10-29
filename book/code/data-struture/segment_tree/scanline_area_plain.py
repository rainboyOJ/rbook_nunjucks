# 扫描线线段树（朴素版）：坐标范围不大时直接按坐标建树，不做离散化。
# 这个结构不需要懒标记下传：cover 只挂在整段被覆盖的节点上，
# 每个节点的覆盖长度由自己的 cover 和孩子信息当场算出。
# 与 C++ 版一一对应：全局列表 cover / len + init(n) + 独立函数。

cover: list[int] = []  # cover[p]：节点 p 管辖区间被完整覆盖的次数
len_: list[int] = []   # len_[p]：节点 p 管辖区间内被覆盖的长度
n: int = 0             # 坐标段数


def init(size: int) -> None:
    """重置为坐标段数 size；换一组数据前必须先调用。"""
    global n, cover, len_
    n = size
    cover = [0] * (n * 4 + 5)
    len_ = [0] * (n * 4 + 5)


def ls(p: int) -> int:
    return p << 1  # 左儿子 2p


def rs(p: int) -> int:
    return p << 1 | 1  # 右儿子 2p+1


def pushup(p: int, l: int, r: int) -> None:
    """上推：cover > 0 说明整段被覆盖；否则由两个孩子拼起来。"""
    if cover[p] > 0:
        len_[p] = r - l + 1
    elif l == r:
        len_[p] = 0  # 叶子且没被完整覆盖，长度为 0
    else:
        len_[p] = len_[ls(p)] + len_[rs(p)]


def add(ql: int, qr: int, v: int, p: int, l: int, r: int) -> None:
    """给 [ql, qr] 增加覆盖次数 v（+1 入边，-1 出边）。"""
    if qr < l or r < ql:
        return
    if ql <= l and r <= qr:
        cover[p] += v
        pushup(p, l, r)
        return
    mid = (l + r) >> 1
    add(ql, qr, v, ls(p), l, mid)
    add(ql, qr, v, rs(p), mid + 1, r)
    pushup(p, l, r)


def covered_len() -> int:
    """整棵树（根节点）的覆盖长度，即当前这一层被覆盖的总长度。"""
    return len_[1]
