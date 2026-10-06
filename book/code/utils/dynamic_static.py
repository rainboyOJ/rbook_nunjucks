# 动态化静态（内存池）：一次性申请 n 个槽位，get() 顺序发放下标，
# 使用方式仍和静态数组一样按下标写 head[i]，但空间来自堆。
# 边界：最多发放 n 个下标；超出后 C++ 是越界 UB，这里显式抛 IndexError。
# Python 有 GC，C++ 析构里的 delete[] head 无需手动释放。


class dynamic_static:
    """固定容量对象池：head[i] 是第 i 个被发放的槽位。"""

    head: list[object]
    idx: int

    def __init__(self, n: int = 10000) -> None:
        # 对应模板参数 N 的默认值 10000；T 在 Python 里没有类型约束。
        self.head = [None] * n
        self.idx = 0

    def get(self) -> int:
        """返回下一个空闲下标并后移指针，即 C++ 的 idx++。"""
        if self.idx >= len(self.head):
            raise IndexError("dynamic_static 池已耗尽")
        pos = self.idx
        self.idx += 1
        return pos
