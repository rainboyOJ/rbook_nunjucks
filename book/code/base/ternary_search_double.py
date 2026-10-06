# 实数三分：在单峰函数（先严格增后严格减，或反过来）上找极值点。
# 每轮把区间缩到原来的 2/3，200 轮远超 double 精度极限（约 1e-10 相对精度）。
# 用的是"比较 f(m1) 与 f(m2)"而不是收敛阈值，迭代次数固定 200，与 C++ 一致。


def f(x: float) -> float:
    # 示例单峰函数：抛物线，峰在 x = 3.0 处，峰值 10.0；调用方可替换自己的 f。
    return -(x - 3.0) * (x - 3.0) + 10.0


def ternary_search(left: float, right: float) -> tuple[float, float]:
    # 返回（极值点估计 x, f(x)）；迭代 200 轮后取中点，与 C++ main 里的写法一致。
    for _ in range(200):
        m1 = left + (right - left) / 3.0
        m2 = right - (right - left) / 3.0

        # f(m1) < f(m2)：极值点（本函数为最大值）必在 m1 右侧，舍去 [left, m1]。
        if f(m1) < f(m2):
            left = m1
        else:
            right = m2

    x = (left + right) / 2.0
    return x, f(x)
