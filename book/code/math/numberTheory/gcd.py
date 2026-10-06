# 辗转相除法求最大公约数，结果非负；gcd(0, 0) = 0。
# 先取绝对值，所以负数输入也能用（C++ 对 INT_MIN 取负会溢出，Python int 任意精度）。
# 调用方保证 |a|、|b| 在 C++ int 范围内；Python 版本没有范围限制。


def gcd(a: int, b: int) -> int:
    if a < 0:
        a = -a
    if b < 0:
        b = -b
    while b != 0:
        # 用 Python 的元组赋值同时更新，等价于 C++ 里的临时变量 r。
        a, b = b, a % b
    return a
