struct TarjanCut {
    int n, timer; // timer 对应你代码中的 cnt
    int dfn[maxn], low[maxn];
    bool is_cut[maxn]; // 标记是否为割点 (对应 cut[])
    int root; // 当前 DFS 树的根节点

    void set(int _n) {
        n = _n;
        timer = 0;
        memset(dfn, 0, sizeof(dfn));
        memset(low, 0, sizeof(low));
        memset(is_cut, 0, sizeof(is_cut));
    }

    /*
     * u: 当前节点
     * fa: u 的父节点 (防止走回头路)
     */
    void dfs(int u, int fa = -1) {
        dfn[u] = low[u] = ++timer;
        int child = 0; // 记录 root 在 DFS 树中的子节点数量

        for (int i = e(i); ~i; i = e[i].next) {
            int v = e[i].v;

            // 不走父子边
            if (v == fa) continue;

            // v 没有被访问过，是树边
            if (!dfn[v]) { 
                child++;
                dfs(v, u); // 【注意】这里必须传 u，表示 v 的父亲是 u

                // 回溯时用子节点的 low 更新当前节点的 low
                low[u] = std::min(low[u], low[v]);

                // 割点判定情况 2: 非根节点
                // 如果子节点 v 无法回到 u 的祖先 (low[v] >= dfn[u])
                // 说明 u 是连接 v 子树和其他部分的必经点
                if (low[v] >= dfn[u] && u != root) {
                    is_cut[u] = true;
                }

                // 处理返祖边
                // 注意:v可能是u的父亲,但没有关系,最多low[u] == dfn[fa[u]]
                // dfn[v] < dfn[u] 说明v是u的祖先,
                // 在无向图上其实不可能 dfn[v] > dfn[u]
                // 因为: v 是一个已经访问过的点, 如果dfn[v] > dfn[u] 说明u是v的祖先, 那么v在u的子树上,
                // 根据dfs 的性质, 应该先访问u, 再访问v,但此时v已经被访问, 所以不可能出现dfn[v] > dfn[u]的情况
            } else if( dfn[v] < dfn[u] ) {
                // v 已经被访问过，是返祖边 (Back Edge)
                // 用 v 的 dfn 更新 u 的 low
                low[u] = std::min(low[u], dfn[v]);
                // 注意：有些版本写成 min(low[u], low[v]) (对于已访问的 v) 是错误的。
                // 对于返祖边，只能取 dfn[v]，因为 low[v] 可能包含从 v 回到更上层的路径，
                // 而边 (u, v) 并不代表 u 可以通过 v 再跳跃回去（除非有其他边）。
            }
        }

        // 割点判定情况 1: 根节点
        // 如果根节点在 DFS 树中有两个及以上的子节点，则它是割点
        if (u == root && child > 1) {
            is_cut[u] = true;
        }
    }

    void solve() {
        // 遍历所有点，防止图不连通
        for (int i = 1; i <= n; i++) {
            if (!dfn[i]) {
                root = i; // 记录当前连通块的 DFS 根
                dfs(i, 0); 
            }
        }
    }
    
    // 辅助函数：获取所有割点
    std::vector<int> get_cuts() {
        std::vector<int> cuts;
        for (int i = 1; i <= n; i++) {
            if (is_cut[i]) cuts.push_back(i);
        }
        return cuts;
    }
};