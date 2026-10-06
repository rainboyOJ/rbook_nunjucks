# 分块（SqrtDecomposition）：区间加、区间和取模查询。
# 与 C++ 版一致：构造时传入 1 下标数组 init（init[0] 不用，长度 n+1），
# 内部自动按 sqrt(n) 分块；add 是区间加，query_mod 是区间和再取模。
# 注意 C++ 版语义：query_mod(l, r, mod) 里传入的 mod 已经是 c+1
# （见源文件 main），调用方需保持同一约定。
# 调用示例：ds = SqrtDecomposition([0, 3, 1, 4]); ds.add(1, 2, 5); ds.query_mod(2, 3, 7)
# Python 的 int 是任意精度，C++ 的 i64 / 1LL 防溢出写法在这里无需担心。
# block_count 可为 0（n = 0 时），此时 add/query_mod 不应被调用（区间为空）。

import math

type Seq = list[int]  # 长度 n+1 或 block_count+1 的 int 序列


class SqrtDecomposition:
    """分块：block_size 为块长，block_count 为块数，1 <= 块编号 <= block_count。"""

    n: int
    block_size: int
    block_count: int
    a: Seq
    block_sum: Seq
    lazy_add: Seq
    belong: Seq
    left_bound: Seq
    right_bound: Seq

    def __init__(self, init: Seq) -> None:
        self.n = len(init) - 1  # C++ 同样按"传入数组长度 - 1"取 n
        self.block_size = max(1, math.isqrt(self.n))  # max(1, ...) 防 n=0 时块长为 0
        self.block_count = (self.n + self.block_size - 1) // self.block_size  # 上取整

        self.a = list(init)  # 复制一份，避免与调用方的数组共享引用
        self.block_sum = [0] * (self.block_count + 1)
        self.lazy_add = [0] * (self.block_count + 1)
        self.belong = [0] * (self.n + 1)
        self.left_bound = [0] * (self.block_count + 1)
        self.right_bound = [0] * (self.block_count + 1)

        for b in range(1, self.block_count + 1):
            self.left_bound[b] = (b - 1) * self.block_size + 1
            self.right_bound[b] = min(self.n, b * self.block_size)
            for i in range(self.left_bound[b], self.right_bound[b] + 1):
                self.belong[i] = b
                self.block_sum[b] += self.a[i]

    def add(self, l: int, r: int, v: int) -> None:
        """区间修改：a[l..r] 整体加 v。

        中间整块只更新 lazy_add 和 block_sum（不逐点落到 a 上），
        散块才逐点加并同步块和——这是分块的核心不变量。
        """
        lb = self.belong[l]
        rb = self.belong[r]

        if lb == rb:
            for i in range(l, r + 1):
                self.a[i] += v
                self.block_sum[lb] += v
            return

        for i in range(l, self.right_bound[lb] + 1):
            self.a[i] += v
            self.block_sum[lb] += v

        for b in range(lb + 1, rb):
            self.lazy_add[b] += v
            self.block_sum[b] += (self.right_bound[b] - self.left_bound[b] + 1) * v

        for i in range(self.left_bound[rb], r + 1):
            self.a[i] += v
            self.block_sum[rb] += v

    def query_mod(self, l: int, r: int, mod: int) -> int:
        """区间查询：a[l..r] 的和，边累加边取模。

        C++ 用 lambda add_mod 逐步取模防溢出；Python int 任意精度，
        但保留逐步取模写法以保持语义一致（结果相同）。
        """
        lb = self.belong[l]
        rb = self.belong[r]
        ans = 0

        if lb == rb:
            for i in range(l, r + 1):
                ans = (ans + self.a[i] + self.lazy_add[lb]) % mod
            return ans

        for i in range(l, self.right_bound[lb] + 1):
            ans = (ans + self.a[i] + self.lazy_add[lb]) % mod

        for b in range(lb + 1, rb):
            ans = (ans + self.block_sum[b]) % mod

        for i in range(self.left_bound[rb], r + 1):
            ans = (ans + self.a[i] + self.lazy_add[rb]) % mod

        return ans
