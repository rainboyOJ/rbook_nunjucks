#include <bits/stdc++.h>
using namespace std;

const int MAXN = 3005;
const double INF = 1e9; 

// 邻接表存图 (目标点, 权值)
// 如果是 P3199 这种带参数的，权值计算可能在 dfs 内部动态算
struct Edge {
    int to;
    long long w; 
};
vector<Edge> adj[MAXN];

// 建图辅助函数
void add_edge(int u, int v, long long w) { adj[u].push_back({v, w}); }

long long dis[MAXN];    // 记录距离
bool vis[MAXN]; 
bool instack[MAXN];  // 【核心】记录当前递归栈中的节点 (是否是祖先)
int n;               // 节点数量

// 【核心函数】DFS-SPFA
// u: 当前节点
// 返回值: true 表示发现负环, false 表示安全
bool spfa_dfs(int u) {
    vis[u] = true;     // 标记 u 已经被访问过
    instack[u] = true; // 1. 入栈：标记 u 在当前路径上

    for (auto &e : adj[u]) {
        int v = e.to;
        double w = e.w; // 注意：如果是01分数规划，这里通常是 e.w - x

        // 2. 松弛判断：只有能让距离变小，才继续走
        if (dis[v] > dis[u] + w) {
            dis[v] = dis[u] + w;

            // 3. 撞到祖先：如果 v 已经在栈里，说明在这个路径上绕回来了
            // 且满足松弛条件，说明环的权值和 < 0
            if (instack[v]) return true;

            // 4. 递归：继续往深处找，如果子路径发现环，直接上报
            if (spfa_dfs(v)) return true;
        }
    }

    instack[u] = false; // 5. 回溯：离开 u，出栈
    return false;
}

// 【驱动函数】处理非连通图
bool has_negative_cycle() {
    // 初始化
    // 技巧：判负环不需要 dis 初始化为 INF，全 0 即可
    // 这相当于建立了一个超级源点连向所有点
    for (int i = 1; i <= n; i++) {
        dis[i] = 0;
        instack[i] = false;
    }

    // 遍历所有点，防止图不连通
    // 注意：这里不需要 visited 数组来跳过，因为 dis 变小才会进 dfs
    // 只要有松弛机会，就应该去尝试，以免漏掉负环
    for (int i = 1; i <= n; i++) {
        if(vis[i]) continue; // 如果已经访问过，就跳过
        if (spfa_dfs(i)) return true;
    }
    return false;
}


// 清空函数 (多测时使用)
void spfa_dfs_init(int _n) {
    n = _n;
    for (int i = 0; i <= n; i++) adj[i].clear();
}