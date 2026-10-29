# 动态开点线段树：值域可以很大（如 1e9），只在真正访问到的位置分配节点。
# 空间 O(m log V)：m 次修改，每次最多新增 log V 个节点。
# 与 C++ 版一一对应：全局列表 lc / rc / sum（下标即指针）+ 独立函数。
# C++ 用 int& 引用出参回写根，Python 没有引用传参，update 改为返回新根，
# 调用方须写 root = update(root, ...)。
# 递归深度 O(log(值域))，1e18 也只有约 60 层，安全。

lc: list[int] = [0]   # lc[p]：p 的左儿子下标，0 表示空
rc: list[int] = [0]   # rc[p]：p 的右儿子下标，0 表示空
sum_: list[int] = [0]  # sum_[p]：p 管辖区间内所有数之和
root: int = 0          # 根节点下标，0 表示空树
tot: int = 0           # 已分配的节点数，0 号节点固定为空节点


def init() -> None:
    """清空整棵树，换一组数据前必须先调用。"""
    global lc, rc, sum_, root, tot
    lc = [0]
    rc = [0]
    sum_ = [0]
    root = 0
    tot = 0


def new_node() -> int:
    """新建一个空节点并返回它的下标。"""
    global tot
    tot += 1
    lc.append(0)
    rc.append(0)
    sum_.append(0)
    return tot


def update(p: int, l: int, r: int, x: int, value: int) -> int:
    """单点加：在值域区间 [l, r] 上把 x 位置加 value，返回该子树的根。"""
    if p == 0:
        p = new_node()  # 走到空节点就开一个，这是“动态开点”
    if l == r:
        sum_[p] += value
        return p
    mid = l + ((r - l) >> 1)  # 防溢出的中点写法
    if x <= mid:
        lc[p] = update(lc[p], l, mid, x, value)
    else:
        rc[p] = update(rc[p], mid + 1, r, x, value)
    sum_[p] = sum_[lc[p]] + sum_[rc[p]]
    return p


def query(p: int, l: int, r: int, ql: int, qr: int) -> int:
    """区间和：节点为空说明整段都是 0，直接返回，绝不在查询里开点。"""
    if p == 0 or qr < l or r < ql:
        return 0
    if ql <= l and r <= qr:
        return sum_[p]
    mid = l + ((r - l) >> 1)
    answer = query(lc[p], l, mid, ql, qr)
    answer += query(rc[p], mid + 1, r, ql, qr)
    return answer
