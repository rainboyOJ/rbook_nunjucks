# 字典树 Trie：字符集固定小写字母 'a'..'z'（26 个）。
# 节点编号 0 为根，同时充当"空指针"（0 表示子节点不存在），新节点从 1 开始编号。
# 用法：tr = Trie(); tr.insert("abc"); tr.contains("abc"); tr.count_prefix("ab")
# 注意：C++ 成员 pass 是 Python 关键字，改名 pass_count，语义不变（经过次数）。

type Nodes = list["Trie.Node"]  # 节点池，下标即编号，tree[0] 为根


class Trie:
    """tree[0] 是根；insert 维护 pass_count，end 只统计"完整单词结尾"。"""

    OFFSET: int = ord("a")  # 'a' 的 ASCII 码，用于把字符映射到 0..25

    class Node:
        """ch[c] 为子节点编号（0 表示不存在）。"""

        def __init__(self) -> None:
            self.ch: list[int] = [0] * 26  # 26 个小写字母的转移
            self.pass_count: int = 0  # 经过该节点的字符串个数
            self.end: int = 0  # 以该节点结尾的完整字符串个数

    tree: Nodes

    def __init__(self) -> None:
        self.tree = [Trie.Node()]  # 建出根节点

    def insert(self, s: str) -> None:
        """插入 s；重复插入会同时增加沿途 pass_count 与末尾 end。"""
        u = 0  # 从根出发
        self.tree[u].pass_count += 1
        for cc in s:
            c = ord(cc) - self.OFFSET
            if self.tree[u].ch[c] == 0:  # 无子节点则新建
                self.tree[u].ch[c] = len(self.tree)
                self.tree.append(Trie.Node())
            u = self.tree[u].ch[c]
            self.tree[u].pass_count += 1
        self.tree[u].end += 1

    def contains(self, s: str) -> bool:
        """s 是否作为完整单词插入过（仅是前缀不算）。"""
        u = 0
        for cc in s:
            c = ord(cc) - self.OFFSET
            if self.tree[u].ch[c] == 0:
                return False
            u = self.tree[u].ch[c]
        return self.tree[u].end > 0  # 必须落在单词结尾

    def count_prefix(self, prefix: str) -> int:
        """统计以 prefix 为前缀的字符串个数（含 prefix 本身）。"""
        u = 0
        for cc in prefix:
            c = ord(cc) - self.OFFSET
            if self.tree[u].ch[c] == 0:
                return 0
            u = self.tree[u].ch[c]
        return self.tree[u].pass_count
