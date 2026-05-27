// ------------------- 模板开始 -------------------
const int MAXN = 2005; // 根据题目要求修改，一般二分图匹配 N<=2000 左右
vector<int> adj[MAXN]; // 邻接表存图：adj[u] 存的是左边点 u 能连到的右边点 v
int match[MAXN];       // match[v] = u：表示【右边】的点 v 当前匹配了【左边】的点 u
bool vis[MAXN];        // vis[v]：在这一轮 DFS 中，右边的点 v 是否被询问过

// 添加边：u 是左边集合的点，v 是右边集合的点
void add_edge(int u, int v) {
    adj[u].push_back(v);
}

// DFS 寻找增广路
// 这里的 u 是左边的点
bool dfs(int u) {
    // 遍历 u 所有可能的“暧昧对象” v (右边的点)
    for (int v : adj[u]) {
        if (vis[v]) continue; // 这一轮已经问过 v 了，不要重复问
        vis[v] = true;        // 标记：这一轮我问过 v 了

        // 核心逻辑：
        // 1. match[v] == 0: v 还是单身，直接匹配！
        // 2. dfs(match[v]): v 已经有对象了(是 match[v])，那我们要去尝试
        //    让 v 的现任对象 (match[v]) 去找别人 (递归查找)。
        //    如果 v 的现任能找到下家，那 v 就空出来了，u 就可以和 v 匹配。
        if (match[v] == 0 || dfs(match[v])) {
            match[v] = u; // 协商成功，v 现在属于 u
            return true;
        }
    }
    return false; // 找了一圈没找到
}

// 匈牙利算法主函数
int hungarian(int n) { // n 是左边集合点的数量
    int ans = 0;
    // match 数组初始化为 0，表示右边点都没匹配
    // 注意：如果点编号包含 0，则需要初始化为 -1
    memset(match, 0, sizeof(match)); 
    
    // 依次尝试给左边的每一个点 i 找对象
    for (int i = 1; i <= n; i++) {
        // 每次开始找增广路前，重置右边点的访问状态
        // 也就是新的一轮“协商”开始了
        memset(vis, 0, sizeof(vis)); 
        if (dfs(i)) {
            ans++;
        }
    }
    return ans;
}
// ------------------- 模板结束 -------------------