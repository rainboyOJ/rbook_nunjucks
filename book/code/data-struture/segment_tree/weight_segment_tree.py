# 权值线段树（动态开点）：按值域统计每个数出现的次数，
# 支持插入、删除、排名、第 k 小、前驱与后继。
# 与 C++ 版一一对应：全局列表 lc / rc / cnt（下标即指针）+ 独立函数。
# 节点 0 是空哨兵：lc[0] = rc[0] = cnt[0] = 0，所以“p 为 0”表示这棵子树全空。
# 下标池用 append 动态增长，不必像 C++ 那样先估一个 MAXNODE。

MINV: int = -10000000  # 值域下界
MAXV: int = 10000000   # 值域上界

lc: list[int] = [0]    # lc[p]：p 的左儿子下标，0 表示空
rc: list[int] = [0]    # rc[p]：p 的右儿子下标，0 表示空
cnt: list[int] = [0]   # cnt[p]：p 管辖区间内的元素个数
root: int = 0          # 根节点下标，0 表示空树
tot: int = 0           # 已分配的节点数，0 号节点固定为空节点


def init() -> None:
    """清空整棵权值线段树，换一组数据前必须先调用。"""
    global lc, rc, cnt, root, tot
    lc = [0]
    rc = [0]
    cnt = [0]
    root = 0
    tot = 0


def new_node() -> int:
    """新建一个空节点并返回它的下标。"""
    global tot
    tot += 1
    lc.append(0)
    rc.append(0)
    cnt.append(0)
    return tot


def update(p: int, l: int, r: int, pos: int, delta: int) -> int:
    """在值域区间 [l, r] 上把 pos 的个数增加 delta，返回该子树（可能新建）的根。"""
    if p == 0:
        p = new_node()  # 走到空节点就开一个，这是“动态开点”
    cnt[p] += delta
    if l == r:
        return p
    mid = l + ((r - l) >> 1)  # 防溢出的中点写法
    if pos <= mid:
        lc[p] = update(lc[p], l, mid, pos, delta)
    else:
        rc[p] = update(rc[p], mid + 1, r, pos, delta)
    return p


def query(p: int, l: int, r: int, ql: int, qr: int) -> int:
    """值域 [ql, qr] 内一共有多少个数；p 为 0 时整段都是 0。"""
    if p == 0 or qr < l or r < ql:
        return 0
    if ql <= l and r <= qr:
        return cnt[p]
    mid = l + ((r - l) >> 1)
    answer = query(lc[p], l, mid, ql, qr)
    answer += query(rc[p], mid + 1, r, ql, qr)
    return answer


def count_less(x: int) -> int:
    """小于 x 的元素个数 = 值域 [MINV, x-1] 的计数。"""
    if x <= MINV:
        return 0
    return query(root, MINV, MAXV, MINV, x - 1)


def kth(p: int, l: int, r: int, k: int) -> int:
    """第 k 小（k 从 1 开始）：左儿子够就往左，否则往右并扣掉左半段。"""
    if l == r:
        return l
    mid = l + ((r - l) >> 1)
    if k <= cnt[lc[p]]:
        return kth(lc[p], l, mid, k)
    return kth(rc[p], mid + 1, r, k - cnt[lc[p]])


def add(x: int) -> None:
    """插入一个 x（等价于 C++ 里的 update(root, MINV, MAXV, x, 1)）。"""
    global root
    root = update(root, MINV, MAXV, x, 1)


def remove(x: int) -> None:
    """删除一个 x，要求 x 当前存在。"""
    global root
    root = update(root, MINV, MAXV, x, -1)


def rank_of(x: int) -> int:
    """x 的排名（比 x 小的个数 + 1）。"""
    return count_less(x) + 1


def kth_smallest(k: int) -> int:
    """第 k 小。"""
    return kth(root, MINV, MAXV, k)


def predecessor(x: int) -> int:
    """小于 x 的最大元素。"""
    return kth(root, MINV, MAXV, count_less(x))


def successor(x: int) -> int:
    """大于 x 的最小元素：第 (≤x 的个数 + 1) 小。"""
    return kth(root, MINV, MAXV, count_less(x + 1) + 1)
