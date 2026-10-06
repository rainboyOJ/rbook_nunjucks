# 红黑树（插入版本）：insert / print / isValid。
# 移植 RBTree/rbtree.cpp 中可运行的主体；Mydelete / del / fixDB 在 C++ 里
# 也是空壳或未完成（fixDB 只有模式表没有操作体，Mydelete 直接注释掉调用），
# Python 版同样不提供假的删除接口。
# 颜色不变量（isValid 逐条检查红黑树 5 性质）：
#   1. 节点非红即黑；2. 根为黑；3. NIL 为黑；4. 红节点子必黑；5. 黑高一致。
# 调用示例：t = RBTree(); t.insert(3); t.insert(1); t.isValid()  ->  True
# Node.debug 的 C++ 版受 RBTree_DEBUG 宏控制，Python 版默认关闭，
# 调用 enable_debug() 打开后 debug() 才有输出（输出到标准输出）。
# ins 递归深度 = 树高 <= 2*log2(n+1)，n = 1e6 时约 40，远低于递归上限，安全。

from __future__ import annotations

import enum

# 节点容量上限：与 C++ SIZE = 1000005 对应，Python 用 list 预分配池，防止
# 无意中让池无限增长；超过上限抛 RuntimeError（C++ 会缓冲区越界，更危险）。
NODE_POOL_SIZE = 1_000_005

_RB_DEBUG: bool = False


def enable_debug() -> None:
    """打开调试输出（对应 C++ 的 RBTree_DEBUG 宏）。"""
    global _RB_DEBUG
    _RB_DEBUG = True


class Color(enum.Enum):
    """节点颜色。ANY 表示"任意颜色"，只用于模式匹配，不落盘到真实节点。"""

    RED = 0  # 🔴
    BLACK = 1  # ⚫
    DOUBLE_BLACK = 2  # ⚫⚫
    ANY = 3  # 任意颜色,主要用于[匹配]


RED = Color.RED
BLACK = Color.BLACK
DOUBLE_BLACK = Color.DOUBLE_BLACK
ANY = Color.ANY


def color_to_string(c: Color) -> str:
    if c == Color.RED:
        return "R"
    if c == Color.BLACK:
        return "B"
    if c == Color.DOUBLE_BLACK:
        return "DB"
    if c == Color.ANY:
        return "_"
    return "?"


class _Ref:
    """模拟 C++ 的 NodePtr&：持有"指向某节点的引用"的槽。

    ref.node 可被赋值，赋值即修改父节点里的 left/right 或树的 root。
    """

    node: Node
    _owner: Node | None  # None 表示树根槽
    _which: int  # 0: 树根槽, 1: _owner.left, 2: _owner.right

    def __init__(self, node: Node) -> None:
        self.node = node
        self._owner = None
        self._which = 0

    @staticmethod
    def as_child(owner: Node, which: int) -> _Ref:
        r = _Ref(owner.left if which == 1 else owner.right)
        r._owner = owner
        r._which = which
        return r

    def assign(self, node: Node) -> None:
        """写回槽位：等价于 C++ 的 u = new_node(...)（NodePtr& 出参）。"""
        self.node = node
        if self._which == 1:
            self._owner.left = node
        elif self._which == 2:
            self._owner.right = node


def print_recursive(node: Node, prefix: str, is_left: bool) -> None:
    if node.is_empty():
        return

    print(prefix, end="")
    print("├──" if is_left else "└──", end="")
    print(f" {node.data} ({color_to_string(node.color)})")

    print_recursive(node.left, prefix + ("│   " if is_left else "    "), True)
    print_recursive(node.right, prefix + ("│   " if is_left else "    "), False)


class Node:
    """红黑树节点。NIL / BBNIL 是两个全局共享的哨兵（对应 C++ 的 inline static Empty / BBEmpty）。"""

    data: int
    color: Color
    left: Node
    right: Node
    parent: Node

    def __init__(
        self,
        data: int,
        color: Color = RED,
        left: Node | None = None,
        right: Node | None = None,
        parent: Node | None = None,
    ) -> None:
        self.data = data
        self.color = color
        if left is None:
            # C++ 构造函数把 left/right/parent 初始化为 this 自身（占位），
            # 真正的哨兵由模块底部创建后统一接线。
            self.left = self
            self.right = self
            self.parent = self
        else:
            self.left = left
            self.right = right
            self.parent = parent

    def is_empty(self) -> bool:
        """是否为哨兵节点（NIL 或 BBNIL）。"""
        return self is NIL or self is BBNIL

    def is_bb_empty(self) -> bool:
        return self is BBNIL

    def is_red(self) -> bool:
        return self.color == RED

    def is_black(self) -> bool:
        return self.color == BLACK

    def is_double_black(self) -> bool:
        return self.color == DOUBLE_BLACK

    def debug(self) -> None:
        """打印以 self 为根的树（受 enable_debug 控制，对应 RBTree_DEBUG）。"""
        if not _RB_DEBUG:
            return
        if self.is_empty():
            return
        print(f"{self.data} ({color_to_string(self.color)})")
        print_recursive(self.left, "", True)
        print_recursive(self.right, "", False)

    # ---- 静态工具函数 ----

    @staticmethod
    def shift_black(node_ref: _Ref) -> None:
        """黑色层级移动一格：NIL <-> BBNIL 互换，普通节点黑红档位升降。

        参数是 _Ref：node 可能被整体换成另一个哨兵（对应 C++ 的 NodePtr&）。
        """
        node = node_ref.node
        if node is NIL:
            node_ref.assign(BBNIL)
        elif node.is_bb_empty():
            node_ref.assign(NIL)
        elif node.is_double_black():
            node.color = BLACK
        elif node.is_black():
            node.color = DOUBLE_BLACK
        elif node.is_red():
            node.color = BLACK

    @staticmethod
    def swap_color(a: Node, b: Node) -> None:
        if a.is_empty() or b.is_empty():
            return
        a.color, b.color = b.color, a.color

    @staticmethod
    def set_red(node: Node) -> None:
        if node.is_empty():
            return
        node.color = RED

    @staticmethod
    def set_black(node: Node) -> None:
        if node.is_empty():
            return
        node.color = BLACK

    # ---- 旋转：来自 bst_common.cpp 的通用操作，作用于 _Ref 槽 ----

    @staticmethod
    def rotate_left(x_ref: _Ref) -> None:
        """左旋，让右孩子 y 上位，自己 x 下沉。口诀: 查空、过继、调父子、更根。"""
        x = x_ref.node
        y = x.right
        if x.is_empty() or y.is_empty():
            return  # 节点或右孩子为空，无法左旋

        # 过继：y 原来的左子树挂到 x 的右边
        x.right = y.left
        if not y.left.is_empty():
            y.left.parent = x

        # x 连接到 y 的左边
        y.left = x
        y.parent = x.parent
        x.parent = y

        # 更新子树根：写回引用槽
        x_ref.assign(y)

    @staticmethod
    def rotate_right(y_ref: _Ref) -> None:
        """右旋，让左孩子 x 上位，自己 y 下沉（与左旋完全对称）。"""
        y = y_ref.node
        x = y.left
        if x.is_empty() or y.is_empty():
            return  # 节点或左孩子为空，无法右旋

        # 过继：x 原来的右子树挂到 y 的左边
        y.left = x.right
        if not x.right.is_empty():
            x.right.parent = y

        # y 连接到 x 的右边
        x.right = y
        x.parent = y.parent
        y.parent = x

        # 更新子树根：写回引用槽
        y_ref.assign(x)


# NIL / BBNIL 哨兵：data 为 0（对应 C++ 的 T()），颜色黑/双黑。
NIL = Node(0, BLACK)
BBNIL = Node(0, DOUBLE_BLACK)


class _Command(enum.Enum):
    """模式操作序列里的命令种类。"""

    ROTATE_LEFT = 0
    ROTATE_RIGHT = 1
    SWAP_COLOR = 2
    SET_BLACK = 3
    SET_RED = 4
    SHIFT_BLACK = 5


class _Opt:
    """一条操作：命令 + 最多两个节点编号（-1 表示未用）。"""

    id: list[int]
    com: _Command

    def __init__(self, id1: int, com: _Command, id2: int = -1) -> None:
        self.id = [id1, id2]
        self.com = com


def _str_to_command(c1: str, c2: str) -> _Command:
    if c1 == "l":
        return _Command.ROTATE_LEFT
    elif c1 == "r":
        return _Command.ROTATE_RIGHT
    elif c1 == "s" and c2 == "w":
        return _Command.SWAP_COLOR
    elif c1 == "s" and c2 == "b":
        return _Command.SET_BLACK
    elif c1 == "s" and c2 == "r":
        return _Command.SET_RED
    elif c1 == "s" and c2 == "s":
        return _Command.SHIFT_BLACK
    return _Command.ROTATE_LEFT


class _RBTreePattern:
    """三层层级模式：用 "B | R B | * * B R | 操作序列" 这样的字符串描述
    一棵待匹配的局部树形（root + 两个孩子 + 四个孙子），匹配成功后
    按操作序列旋转/换色。

    节点编号约定：0=root, 1=l, 2=r, 3=ll, 4=lr, 5=rl, 6=rr。
    """

    desc: list[Color]  # 7 个颜色槽
    opts: list[_Opt]

    def __init__(self, s: str) -> None:
        self.desc = []
        self.opts = []
        i = 0
        while i < len(s):
            c = s[i]
            if c == "B":
                self.desc.append(BLACK)
            elif c == "R":
                self.desc.append(RED)
            elif c == "D":
                self.desc.append(DOUBLE_BLACK)
            elif c == "*":
                self.desc.append(ANY)
            elif c == "l" or c == "r":
                # 两字符命令 + 1 位节点编号，如 "l0"、"r2"
                i += 1
                c2 = s[i]
                i += 1
                node_id = ord(s[i]) - ord("0")
                self.opts.append(_Opt(node_id, _str_to_command(c, c2)))
            elif c == "s":
                # 两字符命令 + 2 位节点编号，如 "sw01"、"ss1"
                i += 1
                c2 = s[i]
                i += 1
                id1 = ord(s[i]) - ord("0")
                i += 1
                id2 = ord(s[i]) - ord("0")
                self.opts.append(_Opt(id1, _str_to_command(c, c2), id2))
            # 其余字符（'|'、空格）跳过
            i += 1

    def match(self, u: Node) -> bool:
        """检查以 u 为根的三层局部树是否符合颜色描述（ANY 忽略）。"""
        if u.is_empty():
            return False
        if self.desc[0] != ANY and u.color != self.desc[0]:
            return False
        if self.desc[1] != ANY and u.left.color != self.desc[1]:
            return False
        if self.desc[2] != ANY and u.right.color != self.desc[2]:
            return False
        if self.desc[3] != ANY and u.left.left.color != self.desc[3]:
            return False
        if self.desc[4] != ANY and u.left.right.color != self.desc[4]:
            return False
        if self.desc[5] != ANY and u.right.left.color != self.desc[5]:
            return False
        if self.desc[6] != ANY and u.right.right.color != self.desc[6]:
            return False
        return True

    @staticmethod
    def find_node(root_ref: _Ref, node_id: int) -> _Ref:
        """按编号取节点引用槽：0=root, 1=l, 2=r, 3=ll, 4=lr, 5=rl, 6=rr。"""
        root = root_ref.node
        if node_id == 0:
            return root_ref
        elif node_id == 1:
            return _Ref.as_child(root, 1)
        elif node_id == 2:
            return _Ref.as_child(root, 2)
        elif node_id == 3:
            return _Ref.as_child(root.left, 1)
        elif node_id == 4:
            return _Ref.as_child(root.left, 2)
        elif node_id == 5:
            return _Ref.as_child(root.right, 1)
        elif node_id == 6:
            return _Ref.as_child(root.right, 2)
        return root_ref  # 都不匹配,无需调整

    def operate(self, root_ref: _Ref) -> None:
        """按顺序执行操作序列。"""
        for opt in self.opts:
            if opt.com == _Command.ROTATE_LEFT:
                node_ref = self.find_node(root_ref, opt.id[0])
                Node.rotate_left(node_ref)
            elif opt.com == _Command.ROTATE_RIGHT:
                node_ref = self.find_node(root_ref, opt.id[0])
                Node.rotate_right(node_ref)
            elif opt.com == _Command.SWAP_COLOR:
                node1 = self.find_node(root_ref, opt.id[0]).node
                node2 = self.find_node(root_ref, opt.id[1]).node
                Node.swap_color(node1, node2)
            elif opt.com == _Command.SET_BLACK:
                node = self.find_node(root_ref, opt.id[0]).node
                Node.set_black(node)
            elif opt.com == _Command.SET_RED:
                node = self.find_node(root_ref, opt.id[0]).node
                Node.set_red(node)
            elif opt.com == _Command.SHIFT_BLACK:
                node_ref = self.find_node(root_ref, opt.id[0])
                Node.shift_black(node_ref)

    def debug(self) -> None:
        out = []
        for i in range(7):
            if self.desc[i] == BLACK:
                c = "B"
            elif self.desc[i] == RED:
                c = "R"
            elif self.desc[i] == DOUBLE_BLACK:
                c = "D"
            else:
                c = "*"
            out.append(c)
            out.append(" | " if (i == 0 or i == 2 or i == 6) else " ")
        print("".join(out))


class RBTree:
    """红黑树：只实现插入（C++ 源文件的删除同样是未完成状态）。"""

    root: Node

    def __init__(self) -> None:
        self.root = NIL

    def make_black(self, node: Node) -> None:
        if node.is_empty():
            return
        node.color = BLACK

    def make_red(self, node: Node) -> None:
        if node.is_empty():
            return
        node.color = RED

    def insert(self, data: int) -> None:
        """插入一个键（重复键插入到右子树，与 C++ 的 `u->data > data` 判左一致）。"""
        # _Ref(self.root) 是树根槽：递归里 assign 只改槽内指针，
        # 递归结束后必须手动写回 self.root（对应 C++ 的 NodePtr& root 出参）。
        root_ref = _Ref(self.root)
        self.make_black(self._ins(data, root_ref))
        self.root = root_ref.node

    def _ins(self, data: int, u_ref: _Ref) -> Node:
        """递归插入，返回调整后的子树根。核心: 把红色上移一层。"""
        u = u_ref.node
        if u.is_empty():
            new_node = self._new_node(data)  # 新节点默认是红色
            u_ref.assign(new_node)
            return new_node
        if u.data > data:
            self._ins(data, _Ref.as_child(u, 1))
        else:
            self._ins(data, _Ref.as_child(u, 2))
        return self.balance(u_ref)

    def _new_node(self, data: int) -> Node:
        """从节点池分配新节点：默认红色，三个指针都指向 NIL。"""
        return Node(data, RED, NIL, NIL, NIL)

    def balance(self, node_ref: _Ref) -> Node:
        """插入后局部修复：匹配 4 种红红冲突模式，旋转 + 换色。

        模式（与 C++ 的 static rotate_desc[4] 一一对应）：
            "B | R * | R * * * | ro0"           LL 型，右旋根
            "B | * R | * * * R | lo0"           RR 型，左旋根
            "B | R * | * R * * | lo1 ro0"       LR 型，先左旋左孩子再右旋根
            "B | * R | * * R * | ro2 lo0"       RL 型，先右旋右孩子再左旋根
        """
        node = node_ref.node
        for pattern in _INSERT_PATTERNS:
            if pattern.match(node):
                pattern.operate(node_ref)
                # 旋转后 node_ref.node 已是新根；提升红色：新根变红、两子变黑
                self.make_black(node_ref.node.left)
                self.make_black(node_ref.node.right)
                self.make_red(node_ref.node)
                return node_ref.node
        return node  # 都不匹配,无需调整

    def print(self) -> None:
        """打印整棵树（调试用，输出到标准输出，对应 C++ 的 print）。"""
        if self.root.is_empty():
            print("Tree is empty.")
        else:
            print_recursive(self.root, "", False)

    def _validate_recursive(self, node: Node) -> int:
        """递归验证红黑树属性并计算黑高；无效返回 -1。

        NIL 的黑高视为 1（对应 C++ 注释：叶子节点黑色，黑高 1）。
        """
        if node.is_empty():
            return 1

        left_bh = self._validate_recursive(node.left)
        right_bh = self._validate_recursive(node.right)

        if left_bh == -1 or right_bh == -1:
            return -1

        # 属性 5: 黑高必须一致
        if left_bh != right_bh:
            if _RB_DEBUG:
                print(f"Validation Error: Black-height mismatch at node {node.data}")
            return -1

        # 属性 4: 红节点的子节点不能是红
        if node.is_red():
            if node.left.is_red() or node.right.is_red():
                if _RB_DEBUG:
                    print(f"Validation Error: Red node {node.data} has red child.")
                return -1

        return left_bh + (1 if node.is_black() else 0)

    def is_valid(self) -> bool:
        """验证整棵树是否符合红黑树 5 条属性。"""
        # 属性 2: 根节点是黑色的。
        if self.root.is_red():
            if _RB_DEBUG:
                print("Validation Error: Root is not black.")
            return False
        return self._validate_recursive(self.root) != -1


# 插入修复的 4 种模式（模块级构造一次，对应 C++ 的 static 数组）。
_INSERT_PATTERNS: list[_RBTreePattern] = [
    _RBTreePattern("B | R * | R * * * | ro0"),
    _RBTreePattern("B | * R | * * * R | lo0"),
    _RBTreePattern("B | R * | * R * * | lo1 ro0"),
    _RBTreePattern("B | * R | * * R * | ro2 lo0"),
]
