// 字典树 Trie 模板：插入字符串、判断是否存在、统计前缀出现次数
// 字符集固定为小写字母 'a'..'z'（共 26 个）
// 节点维护 pass(经过次数) 和 end(单词结尾次数)
// 用法：Trie tr; tr.insert("abc"); tr.contains("abc"); tr.count_prefix("ab");
struct Trie {
  struct Node {
    int ch[26];   // ch[c] 子节点编号，0 为空（根也是 0）
    int pass = 0; // 经过该节点的字符串个数
    int end = 0;  // 以该节点结尾的完整字符串个数
  };
  const char OFFSET = 'a';

  vector<Node> tree; // tree[0] 为根

  Trie() { tree.push_back(Node()); }

  // 插入 s
  void insert(const string &s) {
    int u = 0; // root
    tree[u].pass++;
    for (char cc : s) {
      int c = cc - OFFSET;
      if (tree[u].ch[c] == 0) { // 无子节点则新建
        tree[u].ch[c] = (int)tree.size();
        tree.push_back(Node());
      }
      u = tree[u].ch[c];
      tree[u].pass++;
    }
    tree[u].end++;
  }

  // 判断 s 是否完整插入过
  bool contains(const string &s) const {
    int u = 0;
    for (char cc : s) {
      int c = cc - OFFSET;
      if (tree[u].ch[c] == 0) return false;
      u = tree[u].ch[c];
    }
    return tree[u].end > 0; // 必须作为完整单词结尾
  }

  // 统计以 prefix 为前缀的字符串个数
  int count_prefix(const string &prefix) const {
    int u = 0;
    for (char cc : prefix) {
      int c = cc - OFFSET;
      if (tree[u].ch[c] == 0) return 0;
      u = tree[u].ch[c];
    }
    return tree[u].pass;
  }
};
