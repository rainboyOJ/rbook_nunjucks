import sys

"""
Python 竞赛极简快速输入输出 (Fast I/O)

核心特点：
1. sys.stdin.read().split() 一次性将全部输入读入内存并按空白切分，避免循环调用 input()。
2. sys.stdout.write 替代自带多重格式化与参数检查的 print()。
"""

# 一次性读取全部输入并按空格/换行拆分为 Token 列表
def load_all_tokens():
    return sys.stdin.read().split()

# 基于单行的高速输入（适用于内存严格或交互题场景）
readline = sys.stdin.readline

# 高速单值输出并追加指定分隔符（默认换行）
def write(val, end="\n"):
    sys.stdout.write(str(val) + end)
