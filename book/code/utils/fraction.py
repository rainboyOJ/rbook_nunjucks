# 分数类：num/den 恒为最简形式且 den >= 0；den == 0 表示 ±inf，num 规范为 ±1。
# 构造与四则运算后都会 normalize；比较用交叉相乘，Python int 任意精度，
# 不必像 C++ 那样借助 __int128 防 long long 溢出。

from math import gcd


class Fraction:
    """有理数：恒保持最简、分母非负；den == 0 代表无穷（num 为 ±1）。"""

    num: int
    den: int

    def __init__(self, numerator: int = 0, denominator: int = 1) -> None:
        self.num = numerator
        self.den = denominator
        self.normalize()

    def normalize(self) -> None:
        """把符号移到分子、约分；den == 0 时把 num 规范成 ±1（0 归为 +1）。"""
        if self.den == 0:
            # C++ 用 (num >= 0 ? 1 : -1)：num == 0 也算 +inf。
            self.num = 1 if self.num >= 0 else -1
            return
        if self.den < 0:
            # 负号统一交给分子，保证约分前 den > 0。
            self.num = -self.num
            self.den = -self.den
        g = gcd(abs(self.num), abs(self.den))
        if g != 0:
            # g == 0 只在 num == den == 0 时出现，而 den == 0 已提前返回，故此处可整除。
            self.num //= g
            self.den //= g

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, Fraction):
            return NotImplemented
        return self.num == other.num and self.den == other.den

    def __lt__(self, other: "Fraction") -> bool:
        # 交叉相乘比较；C++ 用 __int128 防 long long 溢出，Python 无此问题。
        return self.num * other.den < other.num * self.den

    def __add__(self, other: "Fraction") -> "Fraction":
        return Fraction(self.num * other.den + other.num * self.den,
                        self.den * other.den)

    def __sub__(self, other: "Fraction") -> "Fraction":
        return Fraction(self.num * other.den - other.num * self.den,
                        self.den * other.den)

    def __mul__(self, other: "Fraction") -> "Fraction":
        return Fraction(self.num * other.num, self.den * other.den)

    def __str__(self) -> str:
        # 对应 C++ 的 operator<<：den == 0 打印 inf / -inf，否则打印 num/den。
        if self.den == 0:
            return "inf" if self.num > 0 else "-inf"
        return f"{self.num}/{self.den}"
