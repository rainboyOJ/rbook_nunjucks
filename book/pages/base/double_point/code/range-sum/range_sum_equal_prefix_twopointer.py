# 前缀和排序后按相同值分组，双指针配对，输出所有和等于 target 的 1 下标闭区间。
# 组内元素值相同，任意两下标之差都等于 target；target == 0 时同组内两两配对。
# 组按 (值, 下标) 全序排列，输出顺序与 C++ 版严格一致。
import sys


def main() -> None:
    data = iter(map(int, sys.stdin.buffer.read().split()))
    n = next(data)
    prefix = [(0, 0)] * (n + 1)
    for i in range(1, n + 1):
        prefix[i] = (prefix[i - 1][0] + next(data), i)
    target = next(data)

    prefix.sort()

    groups: list[tuple[int, int]] = []
    i = 0
    while i <= n:
        j = i + 1
        while j <= n and prefix[j][0] == prefix[i][0]:
            j += 1
        groups.append((i, j))
        i = j

    out: list[str] = []
    if target == 0:
        for l, r in groups:
            for i in range(l, r):
                for j in range(i + 1, r):
                    left = min(prefix[i][1], prefix[j][1]) + 1
                    right = max(prefix[i][1], prefix[j][1])
                    out.append(f"{left} {right}")
    else:
        right_group = 0
        for left_group in range(len(groups)):
            right_group = max(right_group, left_group + 1)
            while right_group < len(groups):
                diff = (
                    prefix[groups[right_group][0]][0]
                    - prefix[groups[left_group][0]][0]
                )
                if diff >= target:
                    break
                right_group += 1
            if right_group == len(groups):
                break

            diff = (
                prefix[groups[right_group][0]][0]
                - prefix[groups[left_group][0]][0]
            )
            if diff != target:
                continue

            l1, r1 = groups[left_group]
            l2, r2 = groups[right_group]
            for i in range(l1, r1):
                for j in range(l2, r2):
                    left = min(prefix[i][1], prefix[j][1]) + 1
                    right = max(prefix[i][1], prefix[j][1])
                    out.append(f"{left} {right}")

    if out:
        sys.stdout.write("\n".join(out) + "\n")


if __name__ == "__main__":
    main()
