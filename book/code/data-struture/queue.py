# 定长数组队列（双端可删）：区间 [head, tail)，push 入队尾、pop 出队首、pop_back 删队尾。
# head / tail 只增不减（clear 除外），push 时按下标覆写而非 append，避免 clear 后残留旧元素。
# C++ 用定长数组 q[siz+5]，Python list 按需增长；front/back 在 C++ 返回引用（可赋值），
# Python 只能返回值，需要改队首/队尾请直接操作 q[head] / q[tail-1]。
# 调用示例：q = MyQueue(); q.push(3); q.push(1); q.front() -> 3; q.back() -> 1


class MyQueue[T]:
    """模板参数 T 对应 C++ 的 typename T，siz 上限在 Python 里由 list 动态扩容取代。"""

    def __init__(self) -> None:
        self.q: list[T] = []
        self.head = 0
        self.tail = 0  # 当前队列区间为 [head, tail)

    def clear(self) -> None:
        self.head = 0
        self.tail = 0

    def push(self, x: T) -> None:
        if self.tail < len(self.q):
            self.q[self.tail] = x  # 复用旧槽位，保证 clear 后不会读到历史元素
        else:
            self.q.append(x)
        self.tail += 1

    def pop(self) -> None:
        self.head += 1

    def pop_back(self) -> None:
        # 竞赛中有时需要从队尾删除元素，例如单调队列。
        self.tail -= 1

    def front(self) -> T:
        return self.q[self.head]

    def back(self) -> T:
        return self.q[self.tail - 1]

    def empty(self) -> bool:
        return self.head == self.tail

    def size(self) -> int:
        return self.tail - self.head
