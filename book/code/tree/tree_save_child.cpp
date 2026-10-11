// 树的存储：孩子表示法
// 每个点记录自己的孩子列表 ch[u]，顺手记下父亲 fa[u]，向上向下就都 O(1) 了。
// 枚举 u 的全部孩子只要 O(孩子数)；空间 O(n)，所有列表一共只装 n-1 个孩子。
// 适用：树形 DP、DFS/BFS 遍历、子树统计等"向下走"的问题。
// 如何使用：
// 1. ChildTree ct(n, root);
// 2. ct.add_child(u, v);       // u 是 v 的父亲
// 3. ct.for_each_child(u, f);  // 遍历 u 的所有孩子
// 4. ct.fa[u];                 // 向上找 u 的父亲

#include <vector>

struct ChildTree {
    int n, root;
    std::vector<std::vector<int>> ch;  // ch[u]：u 的孩子，按加入顺序排列
    std::vector<int> fa;               // fa[u]：u 的父亲；fa[root] = 0

    ChildTree(int n = 0, int root = 1)
        : n(n), root(root), ch(n + 1), fa(n + 1, 0) {}

    // u 是 v 的父亲：把 v 挂到 u 的孩子列表末尾
    void add_child(int u, int v) {
        ch[u].push_back(v);
        fa[v] = u;
    }

    // 遍历 u 的所有孩子
    // 例：ct.for_each_child(u, [](int v){ ... });
    template<typename U>
    void for_each_child(int u, U func) {
        for (int v : ch[u]) func(v);
    }
};
