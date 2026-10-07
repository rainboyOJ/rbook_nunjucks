# 演示“包含 / 不包含端点”的区间长度与相对位移语义，无输入，直接输出。
# 与 C++ 版一样按固定顺序打印 4 行结果，便于逐字节核对。
import sys


def distance_exclude_right(i: int, j: int) -> int:
    # [i, j) 的长度：包含 i，不包含 j。
    return j - i


def distance_include_right(i: int, j: int) -> int:
    # [i, j] 的长度：同时包含 i 和 j。
    return j - i + 1


class RelativePosition:
    """保存当前位置 pos，提供“走 n 步 / 数 n 个”两种语义的位移计算。"""

    pos: int

    def __init__(self, pos: int) -> None:
        self.pos = pos

    def move_exclude_current(self, n: int, dir: int) -> int:
        # 不把当前位置算作第 1 个：走满 n 步。
        return self.pos + n * dir

    def move_include_current(self, n: int, dir: int) -> int:
        # 把当前位置算作第 1 个：只走 n - 1 步。
        return self.pos + (n - 1) * dir

    def next_exclude_current(self, n: int) -> int:
        return self.move_exclude_current(n, 1)

    def next_include_current(self, n: int) -> int:
        return self.move_include_current(n, 1)

    def prev_exclude_current(self, n: int) -> int:
        return self.move_exclude_current(n, -1)

    def prev_include_current(self, n: int) -> int:
        return self.move_include_current(n, -1)


def main() -> None:
    print(distance_exclude_right(3, 9))
    print(distance_include_right(3, 9))

    p = RelativePosition(10)
    print(p.next_exclude_current(3))
    print(p.next_include_current(3))


if __name__ == "__main__":
    main()
