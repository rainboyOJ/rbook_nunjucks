# 动态开点线段树：值域 [l, r] 可以很大（如 1e9），只在真正访问的位置分配节点，
# 空间 O(M log V)，M 为修改次数。
# C++ 用 int& 引用出参回写子节点指针，Python 没有引用传参，update 改为返回新子树根，
# 调用方须写 seg.root = seg.update(seg.root, l, r, x, val)。
# 节点 0 是空哨兵，ls[0] = rs[0] = sum[0] = 0，故 p == 0 表示该区间全为 0。
# 递归深度 O(log(r - l))，值域 1e18 也只有约 60 层，安全。
# C++ 的 sum 是 long long，Python int 任意精度，不存在溢出。

type NodePool = list[int]  # ls / rs / sum 这类按节点下标索引的 int 数组


class DynamicSegTree:
    """动态开点线段树，支持单点加与区间和查询。"""

    ls: NodePool  # 左儿子指针（下标），0 表示空
    rs: NodePool  # 右儿子指针（下标），0 表示空
    sum: NodePool  # 节点维护的区间和
    root: int  # 根节点下标，0 表示空树
    tot: int  # 节点分配器计数

    def __init__(self) -> None:
        # C++ 用定长 MAX_NODES 数组；Python 用动态 append，下标 0 固定为空哨兵。
        self.ls = [0]
        self.rs = [0]
        self.sum = [0]
        self.root = 0
        self.tot = 0

    def pushup(self, p: int) -> None:
        self.sum[p] = self.sum[self.ls[p]] + self.sum[self.rs[p]]

    def update(self, p: int, l: int, r: int, x: int, val: int) -> int:
        """单点加：把 x 位置加 val，返回该子树的新根（p 为 0 时会新建节点）。"""
        if p == 0:
            self.tot += 1
            p = self.tot
            self.ls.append(0)
            self.rs.append(0)
            self.sum.append(0)
        if l == r:
            self.sum[p] += val
            return p
        mid = l + ((r - l) >> 1)  # 防溢出的中点写法：l + (r-l)/2
        if x <= mid:
            self.ls[p] = self.update(self.ls[p], l, mid, x, val)
        else:
            self.rs[p] = self.update(self.rs[p], mid + 1, r, x, val)
        self.pushup(p)
        return p

    def query(self, p: int, l: int, r: int, ql: int, qr: int) -> int:
        """区间查询 [ql, qr] 的和；节点不存在说明其表示区间全是 0。"""
        if p == 0:
            return 0
        if ql <= l and r <= qr:
            return self.sum[p]
        mid = l + ((r - l) >> 1)  # 防溢出的中点写法：l + (r-l)/2
        res = 0
        if ql <= mid:
            res += self.query(self.ls[p], l, mid, ql, qr)
        if qr > mid:
            res += self.query(self.rs[p], mid + 1, r, ql, qr)
        return res
