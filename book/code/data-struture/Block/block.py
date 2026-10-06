# 用模块级全局状态实现"分块"（区间加、区间和），与 C++ 版结构一一对应。
# C++ 里 n、a 是外部全局量，struct Block 只持有自己的数组，并在文件尾声明了
# 全局实例 myblock；Python 同样把 n、a 放在模块级，并保留全局实例 myblock。
# 调用前按下述顺序布置（a 为 1 下标，a[0] 不用）：
#   import 后执行：
#   n = 6
#   a = [0, 3, 1, 4, 1, 5, 9]
# 然后通过全局实例调用（无需手工布置 pos/st/ed/sum/addflag，init 会按 n 建好）：
#   myblock.init()          # 按 sqrt(n) 分块，O(n) 算出每块元素和
#   myblock.update(L, R, d) # 区间 [L, R] 整体加 d
#   myblock.query(L, R)     # 返回区间 [L, R] 的和
# 一行示例：myblock.init(); myblock.update(2, 5, 3); myblock.query(1, 6)
# Python 的 int 是任意精度，C++ 版 query 用 long long 防溢出的担心在这里不存在。

import math

n: int = 0
a: list[int] = []


class Block:
    """分块：block 为块长，t 为块数，1 <= 块编号 <= t。"""

    block: int
    t: int
    pos: list[int]  # pos[i]：位置 i 属于哪一块（1 下标，1..t）
    st: list[int]   # st[b]：第 b 块的左端点
    ed: list[int]   # ed[b]：第 b 块的右端点
    sum: list[int]  # sum[b]：第 b 块内"非懒标记"部分的元素和；与内置 sum 同名是刻意保留 C++ 命名
    addflag: list[int]  # addflag[b]：第 b 块的整块懒标记

    def __init__(self) -> None:
        self.block = 0
        self.t = 0
        self.pos = []
        self.st = []
        self.ed = []
        self.sum = []
        self.addflag = []

    def init(self) -> None:
        """按 sqrt(n) 分块并建好各块端点、归属与元素和。

        易错点：最后一个块的结尾要修正为 n，否则 n 不是块长整倍数时越界。
        C++ 用定长 maxn 数组，Python 改为按 n 分配，语义相同。
        """
        self.block = math.isqrt(n)
        self.t = n // self.block
        if n % self.block:
            self.t += 1

        self.st = [0] * (self.t + 1)
        self.ed = [0] * (self.t + 1)
        for i in range(1, self.t + 1):
            self.st[i] = (i - 1) * self.block + 1
            self.ed[i] = i * self.block
        self.ed[self.t] = n  # 修正最后一个块的结尾

        self.pos = [0] * (n + 1)
        for i in range(1, n + 1):
            self.pos[i] = (i - 1) // self.block + 1

        self.sum = [0] * (self.t + 1)
        self.addflag = [0] * (self.t + 1)
        for i in range(1, self.t + 1):
            for j in range(self.st[i], self.ed[i] + 1):
                self.sum[i] += a[j]

    def update(self, left: int, right: int, d: int) -> None:
        """区间修改：a[L..R] 整体加 d。

        同块内逐点加并同步块和；跨块时中间整块只打懒标记（散块必须在
        本函数内逐点落到 a 数组上，否则查询时无法还原）。
        """
        p = self.pos[left]
        q = self.pos[right]
        if p == q:
            for i in range(left, right + 1):
                a[i] += d
                self.sum[p] += d
        else:
            for i in range(p + 1, q):
                self.addflag[i] += d
            for i in range(left, self.ed[p] + 1):
                a[i] += d
                self.sum[p] += d
            for i in range(self.st[q], right + 1):
                a[i] += d
                self.sum[q] += d

    def query(self, left: int, right: int) -> int:
        """区间查询：返回 a[L] + ... + a[R]。

        散块要加上所在块的懒标记，整块用块和 + 懒标记 * 块长。
        """
        ret = 0
        p = self.pos[left]
        q = self.pos[right]
        if p == q:
            for i in range(left, right + 1):
                ret += a[i]
                ret += self.addflag[p]
        else:
            for i in range(p + 1, q):
                ret += self.sum[i]
                ret += self.addflag[i] * (self.ed[i] - self.st[i] + 1)
            for i in range(left, self.ed[p] + 1):
                ret += a[i]
                ret += self.addflag[p]
            for i in range(self.st[q], right + 1):
                ret += a[i]
                ret += self.addflag[q]
        return ret


myblock = Block()  # 对应 C++ 的全局实例 myblock
