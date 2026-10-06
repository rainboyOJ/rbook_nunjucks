# 枚举把 n 个互不相同的球分成 m 个非空无标号盒的所有方案，球号 1 下标。
# 去重手段：1 号球固定在 1 号盒；每处理一个新球，要么放进已有的某个盒，要么新开
# 编号恰好为 used_boxes + 1 的盒——每个划分只有这一种规范表示，所以不会重复。
# 模块级全局量按下面顺序布置，再调用 dfs（等价于 C++ main 的初始化）：
#   stirling_second_enumerate.n = n
#   stirling_second_enumerate.m = m
#   stirling_second_enumerate.box_items = [[] for _ in range(m)]
#   stirling_second_enumerate.box_items[0].append(1)
#   stirling_second_enumerate.answer_count = 0
#   stirling_second_enumerate.res = []
#   stirling_second_enumerate.dfs(2, 1)   # 1 号球已在 1 号盒，从 2 号球继续
# 前置条件 n >= m >= 1（C++ main 对 n < m 或 m <= 0 直接输出 0 并返回，这段守卫
# 属于输入输出层，不进模板）。递归深度为 n，n 逼近 1e5 时需 sys.setrecursionlimit。

type Boxes = list[list[int]]  # 每个盒子里已放的球号
type Answers = list[Boxes]  # 所有划分的快照列表

n: int = 0
m: int = 0
box_items: Boxes = []
answer_count: int = 0
res: Answers = []


def print_answer() -> None:
    # C++ 原版在这里把当前划分直接 cout；模板不含输出，改为存一份深拷贝快照。
    global answer_count
    answer_count += 1
    res.append([box.copy() for box in box_items])


def dfs(ball: int, used_boxes: int) -> None:
    if ball == n + 1:
        if used_boxes == m:
            print_answer()
        return

    # 放进已有的某个盒子。
    for i in range(used_boxes):
        box_items[i].append(ball)
        dfs(ball + 1, used_boxes)
        box_items[i].pop()

    # 新开一个盒子，编号必须是下一个，这样才能保证每个划分只被枚举一次。
    if used_boxes < m:
        box_items[used_boxes].append(ball)
        dfs(ball + 1, used_boxes + 1)
        box_items[used_boxes].pop()
