# 数位 DP（记忆化 DFS）统计 [l, r] 内 windy 数：相邻两位数字之差的绝对值 >= 2。
# 输入：一行两个整数 l r。输出：区间内 windy 数个数（只统计正整数，0 不算）。
# 递归深度等于十进制位数（2*10^9 只有 10 位），无需 sys.setrecursionlimit。
import sys

MAX_LEN = 15  # 10 位够用，15 留余量
MAX_DIGIT = 10

# dp[pos][last]：剩余 pos 位、上一位是 last、且不受 limit/lead 约束时的方案数；-1 表示未算。
dp = [[-1] * MAX_DIGIT for _ in range(MAX_LEN)]
# digit[pos]：上界 x 从低位数起的第 pos 位（1 下标）。
digit = [0] * MAX_LEN


def dfs(pos: int, last: int, limit: bool, lead: bool) -> int:
    if pos == 0:
        return 0 if lead else 1  # 全是前导零说明这个数就是 0，不算 windy 数
    if not limit and not lead and dp[pos][last] != -1:
        return dp[pos][last]

    up = digit[pos] if limit else 9
    res = 0
    for cur in range(up + 1):
        next_limit = limit and cur == digit[pos]
        next_lead = lead and cur == 0
        if lead:
            # 还没填第一个有效数字，last 无意义，任意 cur 都合法
            res += dfs(pos - 1, cur, next_limit, next_lead)
        elif abs(cur - last) >= 2:
            res += dfs(pos - 1, cur, next_limit, False)

    # 只有贴不贴上界、且不在前导零阶段的状态才是可复用的
    if not limit and not lead:
        dp[pos][last] = res
    return res


def solve(x: int) -> int:
    if x <= 0:
        return 0
    length = 0
    while x > 0:
        length += 1
        digit[length] = x % 10
        x //= 10
    return dfs(length, 0, True, True)


def main() -> None:
    data = iter(map(int, sys.stdin.buffer.read().split()))
    l = next(data)
    r = next(data)
    print(solve(r) - solve(l - 1))


if __name__ == "__main__":
    main()
