# 非负十进制整数转二进制字符串：无前导零，0 转成 "0"。
# 边界：n < 0 时循环不执行、结果为空串，返回 ""，与 C++ 版一致（调用方保证 n >= 0）。
# C++ 的 long long 上限约 9.2e18；Python int 任意精度，位数再多也能转换。


def dec2bin(n: int) -> str:
    if n == 0:
        return "0"

    ans: list[str] = []
    while n > 0:
        # n % 2 是当前最低位，转成字符后先放进列表，最后整体反转。
        ans.append(chr(ord("0") + n % 2))
        n //= 2
    ans.reverse()
    return "".join(ans)
