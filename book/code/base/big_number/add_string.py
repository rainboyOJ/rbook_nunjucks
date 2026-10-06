# 非负十进制大整数加法：输入、输出都是不带正负号的数字字符串（负数不在支持范围内）。
# C++ 用 string 逐位模拟是因为 long long 会溢出；Python 的 int 本身任意精度，
# 保留字符串实现便于对照逐位过程，也便于移植到没有大整数的语言。


def add_positive_integer(a: str, b: str) -> str:
    """返回两个非负十进制数字串的和；结果去掉前导零，但至少保留一位（全零时为 "0"）。"""
    if len(a) < len(b):
        a, b = b, a
    a = a[::-1]
    b = b[::-1]

    carry = 0
    ans: list[str] = []
    for i in range(len(a)):
        x = int(a[i])
        y = int(b[i]) if i < len(b) else 0
        s = x + y + carry
        ans.append(str(s % 10))
        carry = s // 10
    if carry:
        ans.append(str(carry))

    # C++ 里进位最多 1、前导零最多来自输入本身；Python 逻辑一致。
    while len(ans) > 1 and ans[-1] == '0':
        ans.pop()
    return ''.join(reversed(ans))
