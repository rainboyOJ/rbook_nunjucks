# 统计 n 个数的合法入栈/出栈操作序列数量（即出栈序列种数，Catalan 数 C_n）。
# 状态 dfs(in_left, in_stack)：in_left 为还没入栈的数量，in_stack 为栈内数量。
# 出口 in_left == 0 时只能一直出栈，方案数为 1。
# n <= 24（C++ 定长 25），结果最大 C_24 约 1.3e13，Python int 无溢出问题。
# 递归深度最多 2n，远低于默认递归上限，无需 sys.setrecursionlimit。
import sys

n = 0
memo: list[list[int]] = []
vis: list[list[bool]] = []


def dfs(in_left: int, in_stack: int) -> int:
    if in_left == 0:
        return 1
    if vis[in_left][in_stack]:
        return memo[in_left][in_stack]
    vis[in_left][in_stack] = True

    ans = 0
    if in_left > 0:
        ans += dfs(in_left - 1, in_stack + 1)  # 入栈
    if in_stack > 0:
        ans += dfs(in_left, in_stack - 1)  # 出栈

    memo[in_left][in_stack] = ans
    return ans


def main() -> None:
    global n, memo, vis
    data = iter(map(int, sys.stdin.buffer.read().split()))
    n = next(data)
    memo = [[0] * (n + 1) for _ in range(n + 1)]
    vis = [[False] * (n + 1) for _ in range(n + 1)]
    print(dfs(n, 0))


if __name__ == "__main__":
    main()
