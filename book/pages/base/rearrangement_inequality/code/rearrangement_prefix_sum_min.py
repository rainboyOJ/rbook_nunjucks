# 升序排序后依次累加并累计每个前缀和，答案 = 所有前缀和之和。
# 该值等于“每次从剩余元素中取最小值加入前缀”的最小总代价。
# 前缀和与总和可能超出 64 位，Python int 任意精度，无 C++ long long 溢出问题。
import sys


def main() -> None:
    data = iter(map(int, sys.stdin.buffer.read().split()))
    n = next(data)
    a = [next(data) for _ in range(n)]

    a.sort()

    prefix = 0
    answer = 0
    for x in a:
        prefix += x
        answer += prefix

    print(answer)


if __name__ == "__main__":
    main()
