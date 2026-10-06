# upd_max / upd_min / upd：按比较器把 a 更新为更优的值（默认 less 取最大、greater 取最小）。
# C++ 用引用传参原地更新 a，Python 没有引用，改为返回更新后的值：
#   ans = upd_max(ans, x)         等价于 C++ 的 upd_max(ans, x);
#   ans = upd_max(ans, x, y, z)   等价于 C++ 的变参版本。
# 标注用 int | float；运行时接受任何可比较类型（C++ 里是模板 T / U）。

from collections.abc import Callable
from operator import lt

# 数值类型：模板实际用于 int / float，bool 视为 int 的子类型。
type Number = int | float
# 比较器：cmp(x, y) 为真表示 x 应被 y 替换（C++ 的 Compare 默认 std::less）。
type Compare = Callable[[Number, Number], bool]


def upd_max(a: Number, b: Number, *rest: Number) -> Number:
    if a < b:
        a = b
    for x in rest:
        if a < x:
            a = x
    return a


def upd_min(a: Number, b: Number, *rest: Number) -> Number:
    if a > b:
        a = b
    for x in rest:
        if a > x:
            a = x
    return a


def upd(a: Number, b: Number, *rest: Number, cmp: Compare = lt) -> Number:
    # cmp 是仅关键字参数：C++ 里 cmp 固定排在变参列表最后，Python 用关键字传更清晰。
    if cmp(a, b):
        a = b
    for x in rest:
        if cmp(a, x):
            a = x
    return a
