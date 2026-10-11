// 树的存储：父亲表示法（parent / 双亲表示法）
// 每个点只记录自己的父亲 fa[u]，根的父亲记为 0。
// 信息方向：向上走一步 O(1)；向下找一个孩子要扫描全部点 O(n)。
// 适用：自底向上递推、按父亲跳祖先、只关心"向上"信息的问题。
// 如何使用：
// 1. ParentTree pt(n, root);
// 2. pt.set_fa(v, u);              // u 是 v 的父亲，即 fa[v] = u
// 3. pt.for_each_ancestor(v, f);   // 从 v 向上依次访问所有真祖先
// 4. pt.depth(v);                  // v 的深度，根的深度为 0

#include <vector>

struct ParentTree {
    int n, root;
    std::vector<int> fa;  // fa[u] = u 的父亲；fa[root] = 0，表示根没有父亲

    ParentTree(int n = 0, int root = 1) : n(n), root(root), fa(n + 1, 0) {}

    // 把 v 的父亲设成 f；注意参数顺序和 fa[v] = f 一致
    void set_fa(int v, int f) { fa[v] = f; }

    // u 是不是根
    bool is_root(int u) { return fa[u] == 0; }

    // 从 v 向上依次访问所有真祖先（不含 v 自己），一路走到根
    // 例：pt.for_each_ancestor(v, [](int u){ ... });
    template<typename U>
    void for_each_ancestor(int v, U func) {
        for (int u = fa[v]; u != 0; u = fa[u]) func(u);
    }

    // v 的深度：从 v 走到根要经过多少条边（树中路径唯一，步数确定）
    int depth(int v) {
        int d = 0;
        for (int u = fa[v]; u != 0; u = fa[u]) d++;
        return d;
    }
};
