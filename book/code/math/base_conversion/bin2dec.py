# 二进制字符串转十进制整数：s 只应由 '0' / '1' 组成，空串返回 0。
# 例："1101" -> 13。
# C++ 用 long long 接收，超过 63 位会溢出；Python int 任意精度，没有这个上限。


def bin2dec(s: str) -> int:
    ans = 0
    for ch in s:
        # 每读一位就把已有结果乘 2（左移一位），再补上这一位的值。
        # 用 ord(ch) - ord("0") 而不是 int(ch)，与 C++ 的 ch - '0' 逐字符对应。
        ans = ans * 2 + (ord(ch) - ord("0"))
    return ans
