# 高斯消元（Gauss-Jordan）解 n 元一次方程组，增广矩阵 a 形状为 n x (n + 1)，0 下标。
# 返回值：0 无解，1 唯一解，2 无穷多解；唯一解时 ans 被就地改写成 n 个解。
# 浮点判零用 EPS = 1e-9；每列选绝对值最大的行当主元，减少舍入误差。
# C++ 里 a 是按值传递、ans 是 vector<double>& 出参；Python 没有引用出参，
# 所以 a 在函数内先复制一份（调用方的矩阵不会被改动），ans 就地清空再填充：
#   ans: list[float] = []
#   status = gaussian_elimination(a, ans)

EPS = 1e-9

type Grid = list[list[float]]  # 增广矩阵：n 行，每行 n + 1 个数
type Vec = list[float]  # 一行 / 解向量


def gaussian_elimination(a: Grid, ans: Vec) -> int:
    # 复制一份，保证调用方传入的矩阵不被修改（对齐 C++ 的按值传递）。
    a = [row[:] for row in a]
    n = len(a)
    row = 0
    where = [-1] * n  # where[col] 记录第 col 列主元所在的行，-1 表示该列无主元

    for col in range(n):
        if row >= n:
            break

        # 选主元：这一列从 row 往下绝对值最大的行。
        pivot = row
        for i in range(row, n):
            if abs(a[i][col]) > abs(a[pivot][col]):
                pivot = i

        if abs(a[pivot][col]) < EPS:
            continue  # 该列主元可视为 0，说明它是自由变量所在的列
        a[pivot], a[row] = a[row], a[pivot]
        where[col] = row

        div = a[row][col]
        for j in range(col, n + 1):
            a[row][j] /= div  # 主元归一化为 1

        # 把这一列其余行都消成 0（Gauss-Jordan，省去单独的回代）。
        for i in range(n):
            if i == row:
                continue
            factor = a[i][col]
            for j in range(col, n + 1):
                a[i][j] -= factor * a[row][j]

        row += 1

    ans.clear()
    ans.extend([0.0] * n)
    for i in range(n):
        if where[i] != -1:
            ans[i] = a[where[i]][n]

    # 把 ans 代回原方程验证：无解时必有一行不成立。
    for i in range(n):
        total = 0.0
        for j in range(n):
            total += ans[j] * a[i][j]
        if abs(total - a[i][n]) > EPS:
            return 0

    for i in range(n):
        if where[i] == -1:
            return 2  # 有自由变量且方程自洽，无穷多解
    return 1
