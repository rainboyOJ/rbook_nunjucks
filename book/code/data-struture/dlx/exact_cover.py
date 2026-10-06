# DLX（Dancing Links，精确覆盖问题的 X 算法）。
# 列头节点编号 0..cols，其中 0 是总表头；普通节点编号从 cols + 1 开始。
# add_node(r, c) 按"任意行序、行内任意列序"添加 1 元素即可，行内会自动串成环。
# solve() 求一组精确覆盖，行号存进 answer；无解返回 False（此时 answer 不可信）。
# solve 是递归实现，最坏深度等于答案行数，题目规模大时注意
# sys.setrecursionlimit（精确覆盖答案行数一般远小于 1e3，默认上限通常够用）。
# 调用示例：dlx = DLX(n * m + m + 5, n, m); dlx.add_node(1, 2); dlx.solve()

type IntSeq = list[int]  # 链表指针数组 / 计数数组等 int 序列


class DLX:
    """Dancing Links：四向十字链表 + 按列大小最小的列优先覆盖。"""

    cols: int
    node_count: int
    left_link: IntSeq
    right_link: IntSeq
    up_link: IntSeq
    down_link: IntSeq
    row_id: IntSeq
    col_id: IntSeq
    col_size: IntSeq
    row_head: IntSeq  # row_head[r] 为行 r 的某个节点编号，-1 表示该行为空
    answer: IntSeq

    def __init__(self, max_nodes: int, max_rows: int, column_count: int) -> None:
        self.cols = column_count
        self.left_link = [0] * (max_nodes + 1)
        self.right_link = [0] * (max_nodes + 1)
        self.up_link = [0] * (max_nodes + 1)
        self.down_link = [0] * (max_nodes + 1)
        self.row_id = [0] * (max_nodes + 1)
        self.col_id = [0] * (max_nodes + 1)
        self.col_size = [0] * (column_count + 1)
        self.row_head = [-1] * (max_rows + 1)
        self.answer = []
        self.init_headers()

    def init_headers(self) -> None:
        """建立 0..cols 的列头环形双向链表：0 为总表头，自指表示空表。"""
        self.node_count = self.cols + 1
        for i in range(self.cols + 1):
            self.left_link[i] = i - 1
            self.right_link[i] = i + 1
            self.up_link[i] = i
            self.down_link[i] = i
        self.left_link[0] = self.cols  # 首尾相接成环
        self.right_link[self.cols] = 0

    def add_node(self, r: int, c: int) -> None:
        """在第 r 行、第 c 列插入一个 1 元素。

        易错点：列方向插到列头正上方（up_link[c] 之后），
        行方向插到行头左侧（left_link[h] 之后），两处都是环状插入。
        """
        x = self.node_count
        self.node_count += 1
        self.row_id[x] = r
        self.col_id[x] = c
        self.col_size[c] += 1

        self.up_link[x] = self.up_link[c]
        self.down_link[x] = c
        self.down_link[self.up_link[c]] = x
        self.up_link[c] = x

        if self.row_head[r] == -1:
            self.row_head[r] = x
            self.left_link[x] = x
            self.right_link[x] = x
        else:
            h = self.row_head[r]
            self.left_link[x] = self.left_link[h]
            self.right_link[x] = h
            self.right_link[self.left_link[h]] = x
            self.left_link[h] = x

    def cover(self, c: int) -> None:
        """覆盖第 c 列：把列头从总表头摘除，并摘掉该列所有节点所在的行。"""
        self.right_link[self.left_link[c]] = self.right_link[c]
        self.left_link[self.right_link[c]] = self.left_link[c]

        i = self.down_link[c]
        while i != c:
            j = self.right_link[i]
            while j != i:
                self.down_link[self.up_link[j]] = self.down_link[j]
                self.up_link[self.down_link[j]] = self.up_link[j]
                self.col_size[self.col_id[j]] -= 1
                j = self.right_link[j]
            i = self.down_link[i]

    def uncover(self, c: int) -> None:
        """撤销 cover(c)：必须严格逆序回滚，否则链表接不回去。"""
        i = self.up_link[c]
        while i != c:
            j = self.left_link[i]
            while j != i:
                self.col_size[self.col_id[j]] += 1
                self.down_link[self.up_link[j]] = j
                self.up_link[self.down_link[j]] = j
                j = self.left_link[j]
            i = self.up_link[i]

        self.right_link[self.left_link[c]] = c
        self.left_link[self.right_link[c]] = c

    def solve(self) -> bool:
        """求一组精确覆盖，成功时行号在 self.answer 中，失败返回 False。

        分支定界：永远选剩余节点数最少的列，保证剪枝效率。
        """
        if self.right_link[0] == 0:  # 所有列都被覆盖（总表头自环）
            return True

        c = self.right_link[0]
        j = self.right_link[c]
        while j != 0:
            if self.col_size[j] < self.col_size[c]:
                c = j
            j = self.right_link[j]
        if self.col_size[c] == 0:  # 存在无法覆盖的空列
            return False

        self.cover(c)
        i = self.down_link[c]
        while i != c:
            self.answer.append(self.row_id[i])
            j = self.right_link[i]
            while j != i:
                self.cover(self.col_id[j])
                j = self.right_link[j]

            if self.solve():
                return True

            j = self.left_link[i]
            while j != i:
                self.uncover(self.col_id[j])
                j = self.left_link[j]
            self.answer.pop()
            i = self.down_link[i]
        self.uncover(c)
        return False
