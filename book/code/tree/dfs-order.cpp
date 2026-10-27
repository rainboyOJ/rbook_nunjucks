#include <bits/stdc++.h>
using namespace std;

const int maxn = 1e5 + 5;

// 不用 struct 的 DFS 序：全局数组 + 自由函数。
// 用法：
//   1. 读边：tree[u].push_back(v); tree[v].push_back(u);
//   2. reset_order();  dfs_order(root, 0);
//   3. 之后直接用 in_ / out_ / sz / dep / order_，并用 is_ancestor / subtree_size 查询。
//
// 编号从 1 开始，根的 in_ 一定是 1；父亲的哨兵点取 0，dep[0] = 0。
// 预处理是递归 DFS，链形树深度可达 n；n 很大时开大栈（ulimit -s unlimited）或改用迭代版。

vector<int> tree[maxn];

int in_[maxn], out_[maxn];  // 子树 u 对应时间戳区间 [in_[u], out_[u]]
int sz[maxn], dep[maxn];    // 子树大小、深度（根的深度为 1）
int order_[maxn];           // order_[i] = 第 i 个被访问的点
int timer;                  // 时间戳计数器，取值 1..n

void reset_order() { timer = 0; }  // 多组数据必须清空，否则编号会接着上一组涨

void dfs_order(int u, int fa) {
    in_[u] = ++timer;  // 进入 u：拿一个时间戳
    order_[timer] = u;
    sz[u] = 1;
    for (int v : tree[u]) {
        if (v == fa) continue;  // 无向边存成双向，跳过父亲就是只往下走
        dep[v] = dep[u] + 1;
        dfs_order(v, u);
        sz[u] += sz[v];
    }
    out_[u] = timer;  // 退出 u：子树全部访问完，timer 就是区间右端点
    // 等价写法：out_[u] = in_[u] + sz[u] - 1;
}

// u 是不是 v 的祖先（u == v 也算）。等价写法：in_[u] <= in_[v] && out_[v] <= out_[u]
bool is_ancestor(int u, int v) { return in_[u] <= in_[v] && in_[v] <= out_[u]; }

int subtree_size(int u) { return out_[u] - in_[u] + 1; }  // 恒等于 sz[u]

// 欧拉序：进入写一次，每从一个儿子返回再写一次，长度 2n-1。
// 用它配合 RMQ（区间深度最小值）可以求 LCA。
int euler[2 * maxn], etot;

void dfs_euler(int u, int fa) {
    euler[++etot] = u;
    for (int v : tree[u]) {
        if (v == fa) continue;
        dfs_euler(v, u);
        euler[++etot] = u;  // 从儿子 v 退回 u 时，再写一次 u
    }
}
