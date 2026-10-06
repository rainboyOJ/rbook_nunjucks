# 把 a 归一到 [0, |m|) 的模意义代表元。C++ 的 % 向零截断，负数会得到负余数，
# 所以要多补一次 |m|；Python 的 % 是向下取模，m > 0 时结果本就在 [0, m)。
# 调用方保证 m != 0（C++ 里 m = 0 是除零 UB）；Python int 任意精度，无溢出问题。


def normalize_mod(a: int, m: int) -> int:
    m = abs(m)
    # C++：long long r = a % m; if (r < 0) r += m;
    # Python 的 % 已经返回非负余数，与上面两步等价。
    return a % m


def move_on_circle(pos: int, step: int, n: int) -> int:
    # 在长度为 n 的环上从 pos 走 step 步（step 可负），下标归一到 [0, n)。
    return normalize_mod(pos + step, n)
