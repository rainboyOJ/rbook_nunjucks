// 与求割点不同，求割边时有两个关键点需要注意：
// 1.判定条件更严格：low[v] > dfn[u] (不能取等号)。
// 因为如果 low[v] == dfn[u]，说明 $v$ 还能回到 $u$，那么 $u-v$ 这条边在环上，
// 不是桥。防止走回头路：在无向图中，判定“不走回头路”时，
// 2.不能简单地判断 v != father，因为两个点之间可能存在重边（多条边）。
// 我们需要通过边的编号来判断——即“不要走刚才进来的那条边的反向边”。


struct TarjanBridge {
    int n, timer;
    int dfn[maxn], low[maxn];
    bool is_bridge[maxe * 2]; // 标记边是否为桥 (注意大小是边数)

    void set(int _n) {
        n = _n;
        timer = 0;
        memset(dfn, 0, sizeof(dfn));
        memset(low, 0, sizeof(low));
        memset(is_bridge, 0, sizeof(is_bridge));
    }

    /*
     * u: 当前节点
     * in_edge: 进入 u 点的那条边的编号 (替代 father 参数)
     * 用于处理重边情况，确保只屏蔽刚才过来的那一条具体的边
     */
    void dfs(int u, int in_edge) {
        dfn[u] = low[u] = ++timer;

        // 遍历以 u 为起点的所有边
        for (int i = e(u); i != -1; i = e[i].next) {
            int v = e[i].v;

            // 判定条件：i 不是“进入 u 的那条边”的反向边
            // in_edge 是进入 u 的边，in_edge ^ 1 是从 u 回去父亲的边
            // 过滤掉刚才过来的那条边的反向边
            if( i == (in_edge ^ 1) ) continue; 

            if (!dfn[v]) { // 树枝边
                dfs(v, i); // 将当前边 i 传入下一层

                low[u] = std::min(low[u], low[v]);

                // 核心：割边判定
                // 必须是 >，不能是 >=。
                // 如果 low[v] > dfn[u]，说明 v 及其子树无法回到 u 或 u 的祖先
                // 
                // 这里的 else if (dfn[v] < dfn[u]) 其实是个很好的防御性编程。
                // 在无向图 DFS 树中，如果 v 已访问且不是父亲，它一定是祖先 (dfn[v] < dfn[u])。
                // 唯一的例外是有重边连接到由 u 刚访问过的子节点（此时 dfn[v] > dfn[u]），
                // 但这种情况不需要更新 low[u]，因为子节点的 low 已经处理过这种情况了。
                if (low[v] > dfn[u]) {
                    is_bridge[i] = is_bridge[i ^ 1] = true;
                }
            }
            else if( dfn[v] < dfn[u] ) { // 回边
                low[u] = std::min(low[u], dfn[v]);
            }
        }
    }

    void solve() {
        // 遍历所有连通分量
        for (int i = 1; i <= n; i++) {
            if (!dfn[i]) {
                // 对于根节点，没有“进入的边”，传入 -1
                // -1 ^ 1 结果是一个很大的负数或特定值，肯定不会等于正常的边索引(>=0)
                dfs(i, -1);
            }
        }
    }

    // 辅助：获取所有桥 (以 u < v 形式返回)
    std::vector<std::pair<int, int>> get_bridges() {
        std::vector<std::pair<int, int>> ans;
        // edge_cnt 是总边数索引，遍历 0 到 edge_cnt-1
        // 每次 += 2 跳过反向边，避免重复添加
        for (int i = 0; i < edge_cnt; i += 2) {
            if (is_bridge[i]) {
                ans.push_back({e[i ^ 1].v, e[i].v}); // u=e[i^1].v, v=e[i].v
            }
        }
        return ans;
    }
};