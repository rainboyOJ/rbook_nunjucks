#include <bits/stdc++.h>
using namespace std;

const int maxn = 1e6 + 5;
const int max_log = 21;  // 2^20 > 1e6，够把整棵树跳到根

// 不用 struct 的倍增 LCA：全部是全局数组 + 自由函数，查的时候直接写 lca(a, b)。
// 用法：
//   1. 读入无向边：tree[u].push_back(v); tree[v].push_back(u);
//   2. init_lca(n, 1);      // n 个点、以 1 为根；题目指定根就传题目给的根
//   3. lca(a, b)            // 最近公共祖先
//      dist(a, b)           // 树上距离（边数）
//      kth_ancestor(a, k)   // a 的第 k 级祖先
//      is_ancestor(u, v)    // u 是不是 v 的祖先
//
// 内存：up 占 maxn * max_log * 4B ≈ 84MB。n 小时把 maxn / max_log 改小即可。
// 预处理是递归 DFS，树退化成链时深度可达 n；n 很大时记得开大栈（ulimit -s）。

using Graph = vector<int>;
Graph tree[maxn];

int n, lg;              // lg = 实际需要的二进制位数，按 n 收紧，不做无用功
int depth[maxn];        // depth[0] = 0，根 depth = 1；0 号点当哨兵
int up[maxn][max_log];  // up[u][j] = u 的 2^j 级祖先，越界一律落到 0

// 递归 DFS：进点时就填好 depth 和 up[u][1..lg-1]。
// 走到 u 时父亲的整行已经算完，所以 u 的 2^j 级祖先可以直接拿来拼。
void dfs(int u, int fa) {
    depth[u] = depth[fa] + 1;  // fa 为 0 时正好得到根 depth = 1
    up[u][0] = fa;
    for (int j = 1; j < lg; ++j) {
        up[u][j] = up[up[u][j - 1]][j - 1];  // 先跳 2^(j-1)，再跳 2^(j-1)
    }
    for (int v : tree[u]) {
        if (v == fa) continue;  // 只跳过父亲，其余都是儿子
        dfs(v, u);
    }
}

// 预处理：清空上一组的残留，再从 root 递归灌满 depth 与 up。
// 多测时每组都要重新调用，否则会读到上一张图的祖先表。
void init_lca(int n_, int root = 1) {
    n = n_;
    lg = 0;
    while ((1 << lg) <= n) ++lg;  // 最长上跳 n-1 层，只用到前 lg 位
    for (int u = 0; u <= n; ++u) {
        depth[u] = 0;
        for (int j = 0; j < lg; ++j) up[u][j] = 0;
    }
    dfs(root, 0);  // 哨兵点 0：depth[0] = 0、up[0][*] = 0，越界都落到这里
}

// u 的第 k 级祖先：k 的二进制里每个 1 位跳一次
int kth_ancestor(int u, int k) {
    for (int j = 0; j < lg; ++j) {
        if ((k >> j) & 1) u = up[u][j];
    }
    return u;
}

int lca(int a, int b) {
    if (depth[a] < depth[b]) swap(a, b);       // 让 a 是较深的点
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
