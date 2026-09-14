"""纯函数式（persistent / immutable）FHQ-Treap。

函数式要点：
1. 树是**值**，不是对象状态 —— 没有 class 持有 root，全是 free function。
2. split / merge 是纯函数：Tree × Key -> Tree，不修改任何已有节点，
   而是返回**结构共享**的新树。
3. 递归代替循环，match 解构代替属性访问。
4. 副产品：天然获得 persistence —— 每个历史版本都还在，可回溯。

代价：每次操作 O(log n) 个新节点，常数比原地修改大。

键约定：键取整数（Num 的离散子集）。因此 split 只需要一个原语 ——
按 "<= v" 分裂，"< v" 用 v - 1 表达。见 split_lt 的说明。
"""

from __future__ import annotations

import random
from typing import Any, NamedTuple


class Node(NamedTuple):
    """不可变节点。sz 在构造时算好，所以 size 是 O(1)。"""

    v: Any
    p: float
    l: Node | None
    r: Node | None
    sz: int


Tree = Node | None


# ---------- 构造 ----------

def size(t: Tree) -> int:
    return t.sz if t is not None else 0


def node(v: Any, p: float, l: Tree, r: Tree) -> Node:
    return Node(v, p, l, r, 1 + size(l) + size(r))


def leaf(v: Any, p: float | None = None) -> Node:
    return Node(v, random.random() if p is None else p, None, None, 1)


# ---------- 核心：split / merge ----------

def split_le(t: Tree, v: Any) -> tuple[Tree, Tree]:
    """左树所有值 <= v，右树所有值 > v。

    唯一的 split 原语，只依赖键的序关系（<=）。
    """
    match t:
        case None:
            return None, None
        case Node(x, p, l, r, _) if x <= v:
            a, b = split_le(r, v)
            return node(x, p, l, a), b
        case Node(x, p, l, r, _):
            a, b = split_le(l, v)
            return a, node(x, p, b, r)


def split_lt(t: Tree, v: Any) -> tuple[Tree, Tree]:
    """左树所有值 < v，右树所有值 >= v。

    由 split_le 导出，依赖整数键的离散性：
        x < v  <=>  x <= v - 1
    即 v 的前驱恰为 v - 1。

    成立条件（键为整数时全部满足）：
      - 键离散，不存在介于 v-1 与 v 之间的值
      - 键可减，v - 1 有意义
    对 float 键不成立（0.3 的前驱不是 -0.7），对 str / tuple 键不可减。
    """
    return split_le(t, v - 1)


# 参考实现：不经 v - 1 的通用版 split_lt。
# 与上面的 split_le(t, v - 1) 在整数键上结果逐节点相同，
# 但只依赖键的序关系（<），不依赖离散性与减法。
# 需要支持 float / str / tuple 等一般全序键时，换用这个版本即可。
#
# def split_lt(t: Tree, v: Any) -> tuple[Tree, Tree]:
#     """左树所有值 < v，右树所有值 >= v（通用版）。"""
#     match t:
#         case None:
#             return None, None
#         case Node(x, p, l, r, _) if x < v:
#             a, b = split_lt(r, v)
#             return node(x, p, l, a), b
#         case Node(x, p, l, r, _):
#             a, b = split_lt(l, v)
#             return a, node(x, p, b, r)


def merge(x: Tree, y: Tree) -> Tree:
    """前提：x 中所有值 <= y 中所有值。按优先级保持大根堆。"""
    match x, y:
        case None, _:
            return y
        case _, None:
            return x
        case Node(vx, px, lx, rx, _), Node(vy, py, ly, ry, _):
            if px > py:
                return node(vx, px, lx, merge(rx, y))
            return node(vy, py, merge(x, ly), ry)


# ---------- 由 split / merge 组合出的操作 ----------

def insert(t: Tree, v: Any) -> Tree:
    a, b = split_le(t, v)
    return merge(merge(a, leaf(v)), b)


def delete(t: Tree, v: Any) -> Tree:
    """删除一个等于 v 的节点；不存在则原样返回（且不复制节点）。"""
    a, rest = split_lt(t, v)      # a: < v ; rest: >= v
    mid, b = split_le(rest, v)    # mid: == v ; b: > v
    if mid is None:
        return t                  # 没找到，返回原树（结构共享）
    mid = merge(mid.l, mid.r)     # 丢弃 mid 的根
    return merge(merge(a, mid), b)


# ---------- 查询：递归，不用循环 ----------

def count_lt(t: Tree, v: Any) -> int:
    match t:
        case None:
            return 0
        case Node(x, _, l, r, _) if x < v:
            return size(l) + 1 + count_lt(r, v)
        case Node(_, _, l, _, _):
            return count_lt(l, v)


def rank(t: Tree, v: Any) -> int:
    """小于 v 的元素个数 + 1。"""
    return count_lt(t, v) + 1


def kth(t: Tree, k: int) -> Any:
    """第 k 小（1-based）。"""
    match t:
        case None:
            raise IndexError(f"kth index {k} out of range")
        case Node(x, _, l, r, _):
            ls = size(l)
            if k <= ls:
                return kth(l, k)
            if k == ls + 1:
                return x
            return kth(r, k - ls - 1)


def pre(t: Tree, v: Any) -> Any | None:
    """严格小于 v 的最大值。"""
    match t:
        case None:
            return None
        case Node(x, _, _, r, _) if x < v:
            s = pre(r, v)
            return x if s is None else s
        case Node(_, _, l, _, _):
            return pre(l, v)


def succ(t: Tree, v: Any) -> Any | None:
    """严格大于 v 的最小值。"""
    match t:
        case None:
            return None
        case Node(x, _, l, _, _) if x > v:
            s = succ(l, v)
            return x if s is None else s
        case Node(_, _, _, r, _):
            return succ(r, v)


# ---------- 遍历 ----------

def to_list(t: Tree) -> list:
    """中序遍历，纯函数。"""
    match t:
        case None:
            return []
        case Node(x, _, l, r, _):
            return to_list(l) + [x] + to_list(r)


def fold_inorder(t: Tree, f, acc):
    """中序 fold，把树当 foldable 用。"""
    match t:
        case None:
            return acc
        case Node(x, _, l, r, _):
            return fold_inorder(r, f, f(fold_inorder(l, f, acc), x))

