# 数位 DP（迭代预处理 + 逐位统计）统计 [l, r] 内 windy 数：相邻两位差的绝对值 >= 2。
# 输入：一行两个整数 l r。输出：区间内 windy 数个数（只统计正整数，0 不算）。
# f[len][first]：长度恰为 len、最高位为 first、且相邻位差 >= 2 的串数量（允许前导零在低位）。
import sys

MAX_LEN = 11  # 2*10^9 只有 10 位，预计算到 11 留余量
MAX_DIGIT = 10

f = [[0] * MAX_DIGIT for _ in range(MAX_LEN + 1)]


def init() -> None:
    for d in range(MAX_DIGIT):
        f[1][d] = 1  # 长度 1 的串没有相邻位约束
    for length in range(2, MAX_LEN + 1):
        for first in range(MAX_DIGIT):
            total = 0
            for nxt in range(MAX_DIGIT):
                if abs(first - nxt) >= 2:
                    total += f[length - 1][nxt]
            f[length][first] = total


def calc(n: int) -> int:
    if n <= 0:
        return 0

    digit = [0]  # digit[0] 是哨兵，digit[1..len] 为低位到高位
    while n > 0:
        digit.append(n % 10)
        n //= 10
    length = len(digit) - 1

    res = 0
    # 1) 位数严格小于 length 的 windy 数，最高位取 1..9（不能有前导零）
    for l in range(1, length):
        for first in range(1, MAX_DIGIT):
            res += f[l][first]
    # 2) 位数等于 length、最高位小于上界最高位的 windy 数
    for first in range(1, digit[length]):
        res += f[length][first]
    # len == 1 时下面的逐位收紧循环不执行（pos 从 0 开始就终止），
    # 而 n >= 1 的一位数字本身必是 windy 数，必须单独计入（否则 calc(9) 会漏掉 9 自身）。
    if length == 1:
        return res + 1
    # 3) 最高位与上界相同，从次高位起逐位收紧
    for pos in range(length - 1, 0, -1):
        for cur in range(digit[pos]):
            if abs(cur - digit[pos + 1]) >= 2:
                res += f[pos][cur]
        # 上界自身在此位已违反 windy 条件，后面不可能再有合法前缀，直接停
        if abs(digit[pos] - digit[pos + 1]) < 2:
            break
        if pos == 1:
            res += 1  # 上界 n 本身也是 windy 数（len >= 2 的情形）

    return res


def main() -> None:
    data = iter(map(int, sys.stdin.buffer.read().split()))
    l = next(data)
    r = next(data)
    init()
    print(calc(r) - calc(l - 1))


if __name__ == "__main__":
    main()
