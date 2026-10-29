# 可持久化数组：每次修改只复制从根到该叶子的那一条链，
# 于是得到一个新版本，旧版本的所有节点原封不动，仍然可查。
# 与 C++ 版一一对应：全局列表 lc / rc / val（下标即指针）+ 独立函数。
# 版本句柄就是根下标：root[v] 为第 v 个版本的根，0 号节点是空哨兵。
# 递归深度是线段树高度 O(log n)，不会触及默认递归上限。

lc: list[int] = [0]   # lc[p]：p 的左儿子下标，0 表示空
rc: list[int] = [0]   # rc[p]：p 的右儿子下标，0 表示空
val: list[int] = [0]  # val[p]：叶子节点存的数组值
tot: int = 0          # 已分配的节点数，0 号节点固定为空节点

base: list[int] = []  # 原始数组，建树时用 base[1..n]
root: list[int] = []  # root[v]：第 v 个版本的根下标
n: int = 0            # 数组长度


def init(size: int, version_count: int) -> None:
    """重置为长度 size、最多 version_count 个版本；换一组数据前必须先调用。"""
    global lc, rc, val, tot, base, root, n
    n = size
    lc = [0]
    rc = [0]
    val = [0]
    tot = 0
    base = [0] * (n + 1)
    root = [0] * (version_count + 1)


def clone(p: int) -> int:
    """复制节点 p（左右儿子与值原样照抄），返回新节点的下标。"""
    global tot
    tot += 1
    lc.append(lc[p])
    rc.append(rc[p])
    val.append(val[p])
    return tot


def seg_build(l: int, r: int) -> int:
    """用 base[1..n] 建树，返回根节点下标。"""
    p = clone(0)
    if l == r:
        val[p] = base[l]
        return p
    mid = (l + r) >> 1
    lc[p] = seg_build(l, mid)
    rc[p] = seg_build(mid + 1, r)
    return p


def update(p: int, l: int, r: int, pos: int, value: int) -> int:
    """在版本 p 上把位置 pos 改成 value，返回新版本的根下标。"""
    q = clone(p)  # 只复制这一个节点，没走到的子树继续和老版本共享
    if l == r:
        val[q] = value
        return q
    mid = (l + r) >> 1
    if pos <= mid:
        lc[q] = update(lc[p], l, mid, pos, value)
    else:
        rc[q] = update(rc[p], mid + 1, r, pos, value)
    return q


def query(p: int, l: int, r: int, pos: int) -> int:
    """查询版本 p 中位置 pos 的值。"""
    if l == r:
        return val[p]
    mid = (l + r) >> 1
    if pos <= mid:
        return query(lc[p], l, mid, pos)
    return query(rc[p], mid + 1, r, pos)
