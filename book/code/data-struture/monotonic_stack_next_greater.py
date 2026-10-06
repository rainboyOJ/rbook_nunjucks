# 单调栈求每个元素右侧第一个更大元素的下标。
# a 为 1 下标（a[0] 占位不用），返回 ans 也是 1 下标，ans[i] 为下标值、0 表示不存在。
# 栈中保存还没找到右侧更大元素的位置；a[st.back()] 从栈底到栈顶严格递减。
# 弹出时机：a[i] 大于栈顶对应值时，栈顶元素右侧第一个更大元素就是 i。
# 调用示例：next_greater([0, 2, 1, 3]) -> [0, 3, 3, 0]

type Arr = list[int]  # 1 下标数组，a[0] / ans[0] 占位不用


def next_greater(a: Arr) -> Arr:
    """返回长度 n+1 的 ans（1 下标），ans[i] = 右侧第一个更大元素下标，不存在为 0。"""
    n = len(a) - 1
    ans = [0] * (n + 1)
    st: Arr = []  # 栈内是下标，对应值单调不增

    for i in range(1, n + 1):
        while st and a[st[-1]] < a[i]:
            ans[st[-1]] = i
            st.pop()
        st.append(i)

    return ans
