/*
代码细节解释：

0. 此代码同时点双连通分量 和 割点. 因为 求 v-bcc 的时候,通常都会问: 哪些点是割点

1. **`std::vector<int> bcc[maxn]`**:
* 与 SCC 不同，SCC 中每个点只属于一个分量，可以用 `id[u]` 数组标记。
* 在点双 (v-BCC) 中，**割点**会同时属于多个 BCC。因此，我们通常用 `vector` 列表来保存每个 BCC 里有哪些点，而不是给每个点打唯一的 ID 标记。


2. **`st.push(u)` 与出栈逻辑**:
* 我们将点入栈。
* 当满足 `low[v] >= dfn[u]` 时，说明找到了一个以 `u` 为“顶端”的双连通分量。
* 我们不断 `pop` 直到弹出 `v`。
* **关键点**：`u` 也是这个分量的一部分，需要 `bcc[...].push_back(u)`，但是 **`u` 不能出栈**。因为 `u` 还是它父节点所在 BCC 的一部分（如果 `u` 不是根），或者是其他子树分支 BCC 的分割点。


3. **`e(u)`**:
* 这里保留了你代码中的 `e(u)` 写法，假设你已经定义了类似 `#define e(u) head[u]` 或者相应的函数来获取邻接表头指针。
这是一个求 **点双连通分量 (v-BCC)** 的模板。


主要区别在于：

1. **无向图 DFS**：需要传入 `father` 参数防止走回头路（或通过边索引判断）。
2. **出栈时机**：SCC 是在回溯完 `u` 后判断 `low[u] == dfn[u]` 出栈；而 BCC 是在处理子节点 `v` 时，若发现 `low[v] >= dfn[u]`，则说明 `v` 及其子树无法绕过 `u` 到达更早的祖先，此时 `u` 和 `v` 的子树构成一个点双。
3. **割点特性**：一个割点 (Articulation Point) 可以属于多个点双连通分量。

*/

struct TarjanBCC {
    int n, timer;
    std::stack<int> st;
    int dfn[maxn], low[maxn];
    int bcc_cnt; // BCC 计数
    bool is_cut[maxn];
    int root; //记录根节点
    std::vector<int> bcc[maxn]; // 存储每个 BCC 包含的具体节点

    void set(int _n) {
        n = _n;
        timer = bcc_cnt = 0;
        memset(dfn, 0, sizeof(dfn));
        memset(low, 0, sizeof(low));
        memset(is_cut,0,sizeof(is_cut));

        while (!st.empty()) st.pop();
        for (int i = 0; i <= n; i++) bcc[i].clear();
    }

    // 无向图，需要 fa 参数防止直接走回父节点
    void dfs(int u, int fa) {
        dfn[u] = low[u] = ++timer;
        st.push(u);
        int child = 0;

        for (int i = e.h[u]; ~i; i = e[i].next) {
            int v = e[i].v;
            
            if (v == fa) continue; // 无向图核心：不走回头路

            if (!dfn[v]) { // 如果 v 没被访问过
                dfs(v, u);
                child++;

                if(u != root  && low[v] >= dfn[u]) {
                    is_cut[u] = 1; 
                }
                
                // 更新 low 值
                low[u] = std::min(low[u], low[v]);

                // 核心判断：low[v] >= dfn[u] 说明 v 没法回到 u 的祖先
                // 此时 u 是割点（或者根），u-v 这条边及其下方的点构成一个 BCC
                if (low[v] >= dfn[u]) {
                    bcc_cnt++;
                    while (true) {
                        int node = st.top(); st.pop();
                        bcc[bcc_cnt].push_back(node);
                        if (node == v) break; // 只要弹到 v 为止
                    }
                    // 注意：u 也是这个 BCC 的一部分，但 u 可能属于多个 BCC，
                    // 所以 u 不能出栈，只是把 u 加入到当前 BCC 列表中
                    bcc[bcc_cnt].push_back(u);
                }
            } else if (dfn[v] < dfn[u]) { // 返祖边
                low[u] = std::min(low[u], dfn[v]);
            }
        }

        if( u == root && child > 1) 
            is_cut[u] =  1;
    }

    void solve() {
        for (int i = 1; i <= n; i++) {
            if (!dfn[i] && e.h[i] != -1 ) {
                // 此时栈为空，dfs 根节点
                // 根节点的特判通常包含在上述 dfs 逻辑中
                // 只有当图中有孤立点时，需额外注意栈内残留
                root = i;
                dfs(i, 0); 
                
                // 如果是孤立点或者根节点处理完栈里还有元素（极少见情况，视题目定义而定）
                // 实际上标准 v-BCC 逻辑中，上述 dfs 里的 if (low[v] >= dfn[u]) 会处理所有连通块
                // 唯一的例外是如果 i 是一个孤立点（没有边的点），它不会进入循环
                // 如果需要记录孤立点为 BCC，可以在这里补判
            }
        }
    }

    // helper 
    int cut_cnt() {
        int cnt = 0;
        for(int i = 1;i <= n ;++i ) // i: 1->n
            cnt += is_cut[i];
        return cnt;
    }
};