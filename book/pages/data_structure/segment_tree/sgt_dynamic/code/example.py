# 动态开点线段树示例，对应同目录 example.cpp。
# 程序不读输入：在值域 [1, 1e9] 上做三次单点加，再查询两个区间。
# 与 example.cpp 一一对应：全局列表 lc / rc / sum_（下标即指针）+ 独立函数。
# 下标 0 表示空节点，节点从 1 开始编号；递归深度 O(log V) ≈ 30，安全。

import sys

lc: list[int] = [0]   # lc[p]：p 的左儿子下标，0 表示空
rc: list[int] = [0]   # rc[p]：p 的右儿子下标，0 表示空
sum_: list[int] = [0]  # sum_[p]：p 管辖区间内所有数之和
root: int = 0          # 根节点下标，0 表示空树
tot: int = 0           # 已分配的节点数，0 号节点固定为空节点


def new_node() -> int:
    """新建一个空节点并返回它的下标。"""
    global tot
    tot += 1
    lc.append(0)
    rc.append(0)
    sum_.append(0)
    return tot


def update(p: int, l: int, r: int, x: int, value: int) -> int:
    """单点加：在值域 [l, r] 上把 x 位置加 value，返回该子树的根编号。

    C++ 用 int& 出参直接改指针，Python 只能把新编号返回给调用方写回。
    """
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
    """区间和：没建过的子树贡献 0，查询里绝不开点。"""
    if p == 0 or qr < l or r < ql:
        return 0
    if ql <= l and r <= qr:
        return sum_[p]
    mid = l + ((r - l) >> 1)
    answer = query(lc[p], l, mid, ql, qr)
    answer += query(rc[p], mid + 1, r, ql, qr)
    return answer


def main() -> None:
    global root
    limit = 10**9

    root = update(root, 1, limit, 1000, 5)
    root = update(root, 1, limit, 2000000, 10)
    root = update(root, 1, limit, 999999999, 20)

    # [1, 10000] 只覆盖 1000 这个修改点。
    sys.stdout.write(str(query(root, 1, limit, 1, 10000)) + "\n")
    # [1, 1e9] 覆盖全部三个修改点。
    sys.stdout.write(str(query(root, 1, limit, 1, limit)) + "\n")
    sys.stdout.write(f"Allocated nodes: {tot}\n")


if __name__ == "__main__":
    main()
