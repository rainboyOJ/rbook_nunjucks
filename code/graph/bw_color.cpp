/*
 * 二分图判定 (黑白染色) BFS版本
 * 原理：尝试给图染色，相邻节点颜色不同。如果遇到冲突，说明存在奇环。
 * 复杂度：O(V + E)
 * 依赖：需要外部定义的 adj (邻接表) 和 color 数组
 */

int color[maxn]; // 0:未访问, 1:颜色A, 2:颜色B

// 对单个连通块进行染色
// start: 连通块的起点
// 返回值: true 是二分图(无冲突), false 不是二分图(有奇环)
bool bfs_bw_color(int start) {
    std::queue<int> q;
    q.push(start);
    color[start] = 1; // 起点染成颜色 1

    while (!q.empty()) {
        int u = q.front(); q.pop();

        // 遍历 u 的所有邻居
        // 如果你用的是链式前向星，请替换下面的遍历方式
        for (int v : e.adj[u]) { 
        // for(int i = h[u]; ~i; i = e[i].next) { int v = e[i].v; ...

            // 如果 v 未被染色
            if (color[v] == 0) {
                color[v] = 3 - color[u]; // 1变2，2变1
                q.push(v);
            }
            // 如果 v 已染色，且颜色与 u 相同 -> 冲突
            else if (color[v] == color[u]) {
                return false; // 发现奇环
            }
        }
    }
    return true;
}

// 主判定函数：处理非连通图
bool is_bipartite(int n) {
    // 初始化颜色数组
    // memset(color, 0, sizeof(color)); 
    // 如果是多组数据，请确保在 init() 中清空了 color

    for (int i = 1; i <= n; i++) {
        // 如果点 i 没有被访问过，则从 i 开始跑 BFS
        if (color[i] == 0) {
            if (!bfs_bw_color(i)) {
                return false; // 只要有一个连通块不是二分图，整张图就不是
            }
        }
    }
    return true;
}