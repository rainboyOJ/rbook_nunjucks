# 手写栈：sta[0 .. top_pos-1] 为栈内元素，top_pos 指向栈顶元素后一位，等价于栈大小。
# 空栈时 top()/pop() 是未定义行为（C++ 读 sta[-1]），调用方须先判 empty()。
# C++ 模板参数 siz 只是定长数组容量，Python 用动态 list 存储，无需容量参数。
# C++ 的 top() 返回 T& 可写，Python 只能返回值，需要改写请 top() 取值后再 push。

class MyStack[T]:
    """后进先出的栈，接口与 C++ 版 MyStack<T> 一一对应。"""

    sta: list[T]  # 栈内元素，仅下标 0 .. top_pos-1 有效
    top_pos: int  # 栈顶元素后一位的下标，同时就是栈的大小

    def __init__(self) -> None:
        self.clear()

    def clear(self) -> None:
        """清空栈。"""
        self.sta = []
        self.top_pos = 0

    def push(self, x: T) -> None:
        self.sta.append(x)
        self.top_pos += 1

    def pop(self) -> None:
        self.sta.pop()
        self.top_pos -= 1

    def top(self) -> T:
        # 返回的是值的副本：Python 没有 C++ 那样的引用返回值。
        return self.sta[self.top_pos - 1]

    def empty(self) -> bool:
        return self.top_pos == 0

    def size(self) -> int:
        return self.top_pos
