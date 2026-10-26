#include <bits/stdc++.h>
using namespace std;

const int maxn = 1e6 + 5;
const int max_log = 21;  // 2^20 > 1e6，够把整棵树跳到根

// 不用 struct 的倍增 LCA：全部是全局数组 + 自由函数，查的时候直接写 lca(a, b)。
// 用法：
//   1. 读入无向边：tree[u].push_back(v); tree[v].push_back(u);
//   2. init_lca(n);        // 单测可省；多测必须调用，用来清空上一组的残留
//   3. build_lca(1);       // 以 1 为根预处理，题目指定根就传题目给的根
//   4. lca(a, b)           // 最近公共祖先
//      dist(a, b)          // 树上距离（边数）
//      kth_ancestor(a, k)  // a 的第 k 级祖先
//      is_ancestor(u, v)   // u 是不是 v 的祖先
//
// 内存：up 占 maxn * max_log * 4B ≈ 84MB。n 小时把 maxn / max_log 改小即可。

using Graph = vector<int>;
Graph tree[maxn];

int n, lg;              // lg = 实际需要的二进制位数，按 n 收紧，不做无用功
int depth[maxn];        // depth[0] = 0，根 depth = 1；0 号点当哨兵
int up[maxn][max_log];  // up[u][j] = u 的 2^j 级祖先，越界一律落到 0
int stk[maxn];          // 手写栈：1e6 深的链递归会爆栈，这里改成迭代

void init_lca(int n_) {
    n = n_;
    lg = 0;
    while ((1 << lg) <= n) ++lg;  // 最长上跳 n-1 层，只用到前 lg 位
    for (int u = 0; u <= n; ++u) {
        depth[u] = 0;
        for (int j = 0; j < lg; ++j) up[u][j] = 0;
    }
}

// 迭代 DFS：出栈时填 up[u][1..lg-1]，因为 u 的父亲和更高祖先早就填好了。
void build_lca(int root = 1) {
    int top = 0;
    depth[0] = 0;  // 哨兵，保证根的父亲深度为 0
    depth[root] = 1;
    up[root][0] = 0;
    stk[top++] = root;
    while (top) {
        int u = stk[--top];
        for (int j = 1; j < lg; ++j) {
            up[u][j] = up[up[u][j - 1]][j - 1];  // 先跳 2^(j-1)，再跳 2^(j-1)
        }
        for (int v : tree[u]) {
            if (v == up[u][0]) continue;  // 只跳过父亲
            up[v][0] = u;
            depth[v] = depth[u] + 1;
            stk[top++] = v;
        }
    }
}

// u 的第 k 级祖先：k 的二进制里每个 1 位跳一次
int kth_ancestor(int u, int k) {
    for (int j = 0; j < lg; ++j) {
        if ((k >> j) & 1) u = up[u][j];
    }
    return u;
}

int lca(int a, int b) {
    if (depth[a] < depth[b]) swap(a, b);      // 让 a 是较深的点
    a = kth_ancestor(a, depth[a] - depth[b]);  // 先跳到同一层
    if (a == b) return a;                      // 已经重合，b 就是答案

    // 从大步到小步：跳到 LCA 的正下方，父节点就是答案
    for (int j = lg - 1; j >= 0; --j) {
        if (up[a][j] != up[b][j]) {
            a = up[a][j];
            b = up[b][j];
        }
    }
    return up[a][0];
}

// 树上两点距离（边数）
int dist(int a, int b) {
    int c = lca(a, b);
    return depth[a] + depth[b] - 2 * depth[c];
}

// u 是 v 的祖先 <=> v 向上爬 depth[v]-depth[u] 层正好落在 u
bool is_ancestor(int u, int v) {
    if (depth[u] > depth[v]) return false;
    return kth_ancestor(v, depth[v] - depth[u]) == u;
}
