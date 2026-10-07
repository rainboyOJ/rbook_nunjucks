# 求满足 i < j 且 j - i <= k 的 a[i] + a[j] 最大值。
# 单调队列维护当前窗口内 a[i] 递减的下标，队首即窗口最大值。
# 队首过期条件 q[0] < j - k 对应 C++ 的 q.front() < j - k。
# 无合法配对（n < 2）时与 C++ 一致输出 LLONG_MIN，故用哨兵 NEG。
import sys
from collections import deque

NEG = -(1 << 63)  # LLONG_MIN，对齐 C++ 初值语义


def main() -> None:
    data = iter(map(int, sys.stdin.buffer.read().split()))
    n = next(data)
    k = next(data)
    a = [next(data) for _ in range(n)]

    q: deque[int] = deque()
    ans = NEG
    for j in range(n):
        while q and q[0] < j - k:
            q.popleft()

        if q:
            ans = max(ans, a[q[0]] + a[j])

        # 队尾元素不可能再作为后续窗口最大值，弹出；保留相等值取更靠右的下标。
        while q and a[q[-1]] <= a[j]:
            q.pop()
        q.append(j)

    print(ans)


if __name__ == "__main__":
    main()
