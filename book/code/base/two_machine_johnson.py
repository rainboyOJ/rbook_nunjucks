# 两台机器流水作业调度（Johnson 法）：按 min(a, b) 排序后顺序通过两台机器。
# 排序规则：a <= b 的"前段作业"在前（按 a 升序），a > b 的"后段作业"在后（按 b 降序）。
# C++ 用 long long 防止 finish_m1 累加溢出 int；Python int 任意精度，该坑不存在。

from functools import cmp_to_key

type Jobs = list[Job]


class Job:
    """id 是输入顺序（1 开始），仅用于输出加工顺序；a、b 是两台机器的加工时长。"""

    def __init__(self, id: int, a: int, b: int) -> None:
        self.id = id
        self.a = a
        self.b = b


def johnson_cmp(x: Job, y: Job) -> bool:
    # 对应 C++ 的严格弱序比较器：返回 True 表示 x 应排在 y 前面。
    x_front = x.a <= x.b
    y_front = y.a <= y.b

    if x_front != y_front:
        return x_front and not y_front
    if x_front:
        return x.a < y.a
    return x.b > y.b


def sort_jobs(jobs: Jobs) -> None:
    # C++ 的 sort 直接吃“是否更靠前”的严格弱序比较器；cmp_to_key 需要三向比较，
    # 这里用 johnson_cmp(x, y) / johnson_cmp(y, x) 拼出 -1/0/1，保持 johnson_cmp 本身与 C++ 同签名。
    # 并列作业（比较器判等）的先后不是契约：std::sort 不稳定、Timsort 稳定，两边可能不同但都合法。
    def three_way(x: Job, y: Job) -> int:
        if johnson_cmp(x, y):
            return -1
        if johnson_cmp(y, x):
            return 1
        return 0

    jobs.sort(key=cmp_to_key(three_way))


def finish_time(jobs: Jobs) -> int:
    finish_m1 = 0
    finish_m2 = 0
    for job in jobs:
        finish_m1 += job.a
        # 机器 2 必须等：作业上机时刻是 max(finish_m2, finish_m1)，再加工 job.b。
        finish_m2 = max(finish_m2, finish_m1) + job.b
    return finish_m2

