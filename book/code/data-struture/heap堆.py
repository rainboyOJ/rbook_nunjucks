# 小根堆（隐式二叉堆）：push / top / pop / size / empty。
# 下标从 1 开始：h[0] 是占位哨兵不使用，从而父子关系恒为 u//2、u*2、u*2+1。
# C++ 模板参数 T 在 Python 里用泛型类表达；list 无容量上限，无需像 C++ 那样预留 maxn。


class MinHeap[T]:
    """h 用列表存节点，h[1] 为堆顶；比较用 <，要求 T 可比较。"""

    def __init__(self) -> None:
        # 0 号位置占位；C++ 里是 h.push_back(T())，Python 用 None 占位且永不访问。
        self.h: list[T] = [None]  # type: ignore[list-item]

    def size(self) -> int:
        return len(self.h) - 1

    def empty(self) -> bool:
        return self.size() == 0

    def top(self) -> T:
        """堆顶（最小值）。调用前需保证非空，否则与 C++ 访问 h[1] 一样越界。"""
        return self.h[1]

    def up(self, u: int) -> None:
        """上浮：节点 u 比父节点小就交换，恢复堆性质。"""
        while u > 1 and self.h[u] < self.h[u // 2]:
            self.h[u], self.h[u // 2] = self.h[u // 2], self.h[u]
            u //= 2

    def down(self, u: int) -> None:
        """下沉：节点 u 与较小的孩子交换，恢复堆性质。"""
        while True:
            best = u
            left = u * 2
            right = u * 2 + 1

            if left <= self.size() and self.h[left] < self.h[best]:
                best = left
            if right <= self.size() and self.h[right] < self.h[best]:
                best = right
            if best == u:
                break

            self.h[u], self.h[best] = self.h[best], self.h[u]
            u = best

    def push(self, x: T) -> None:
        """插入值 x：放到末尾再上浮。"""
        self.h.append(x)
        self.up(self.size())

    def pop(self) -> None:
        """删除堆顶：末尾元素补到根再下沉；空堆时静默返回，与 C++ 一致。"""
        if self.empty():
            return
        self.h[1] = self.h[-1]
        self.h.pop()
        if not self.empty():
            self.down(1)
