# 单调栈求直方图最大矩形面积。
# h 为 1 下标高度数组（h[1..n]），要求高度非负；函数内部按 C++ 版补齐 h[0] = h[n+1] = 0 两个哨兵。
# 对每个柱子 mid，左右两端第一个比它矮的柱子夹出的宽度就是它能撑起的最大宽度；
# 用递增单调栈，遇到更矮的 h[i] 时弹出并结算 mid 的面积。
# C++ 用 long long 防 height * width 溢出 int；Python int 任意精度，该坑不存在。
# 调用示例：largest_rectangle([0, 2, 1, 5, 6, 2, 3]) -> 10

type Arr = list[int]  # 1 下标数组，h[0] 占位不用


def largest_rectangle(h: Arr) -> int:
    """返回直方图中最大矩形面积；h 长度至少为 1（n = len(h) - 1，n = 0 时返回 0）。"""
    n = len(h) - 1

    # 复制成 n+2 长度并补上左右哨兵 0：保证栈底恒有 h[0]=0 兜底，且最后 i=n+1 能弹空栈。
    hs = [0] * (n + 2)
    for i in range(1, n + 1):
        hs[i] = h[i]

    answer = 0
    st: Arr = [0]  # 栈内存下标，对应高度单调不减

    for i in range(1, n + 2):
        while hs[st[-1]] > hs[i]:
            mid = st.pop()
            height = hs[mid]
            width = i - st[-1] - 1  # 左右第一个更矮柱子之间的宽度
            area = height * width
            if area > answer:
                answer = area
        st.append(i)

    return answer
