#include <cstring>
#include <vector>

// 树的存储：父亲孩子表示法
// 每个点记两个方向：fa[u] 是父亲，ch[u] 是孩子列表，向上向下都 O(1)。
// 孩子列表一共只装 n-1 个孩子，总空间 O(n)。
// 适用：树形 DP、DFS/BFS 遍历、换根这类两个方向都要的问题。
// 如何使用：
// 1. init(n);                // 多组数据必须调用
// 2. add_child(u, v);        // u 是 v 的父亲
// 3. 向下：for (int v : ch[u]) ... ；向上：fa[u]

const int maxn = 1e6 + 5;  // 和别的模板一起用时 maxn 只留一份

int fa[maxn];               // fa[u]：u 的父亲；fa[root] = 0，根没有父亲
std::vector<int> ch[maxn];  // ch[u]：u 的孩子，按加入顺序

// 初始化：把 1..n 清空，fa 全为 0（fa[root] = 0 正好表示"根没有父亲"）
void init(int n) {
    for (int i = 0; i <= n; i++) ch[i].clear();
    memset(fa, 0, sizeof(fa));
}

// u 是 v 的父亲：孩子挂到 ch[u]，父亲写进 fa[v]，加边 O(1)
void add_child(int u, int v) {
    ch[u].push_back(v);
    fa[v] = u;
}
