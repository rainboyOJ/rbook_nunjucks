# 向上取整除法，逐字复刻 C++ 的 a / b（向零截断）与 a % b（余数符号随被除数）。
# 正数输入下就是常见的 (a + b - 1) / b；负数输入下 C++ 的写法并不等于数学上的 ceil，
# 这里保持一致、不做"修正"。
# 调用方保证 b != 0（C++ 除零是未定义行为，Python 会抛 ZeroDivisionError）。
# C++ 的 long long 溢出坑在 Python 不存在，int 是任意精度。


def ceil_div(a: int, b: int) -> int:
    # Python 的 // 向下取整，负数时与 C++ 的向零截断差 1，所以先按绝对值求商再补符号。
    quotient = abs(a) // abs(b)
    if (a < 0) != (b < 0):
        quotient = -quotient
    # C++ 的 a % b 就是 a - (a / b) * b，非零则商要进一。
    remainder = a - quotient * b
    return quotient + (remainder != 0)
