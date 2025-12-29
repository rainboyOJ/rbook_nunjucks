struct TarjanScc {
    int n, timer;
    std::stack<int> st;
    bool in_stack[maxn];
    int dfn[maxn], low[maxn], scc_id[maxn];
    int scc_cnt; // SCC 的总数

    void set(int _n) {
        n = _n;
        timer = scc_cnt = 0;
        memset(dfn, 0, sizeof(dfn));
        memset(in_stack, 0, sizeof(in_stack));
    }

    // 有向图,不要加father参数
    void dfs(int u) {
        dfn[u] = low[u] = ++timer;
        st.push(u);
        in_stack[u] = true;

        for (int i = e(u); ~i ; i = e[i].next) {
            int v = e[i].v;
            if (!dfn[v]) { // 如果 v 没被访问过
                dfs(v);
                
                // 根据子节点的 low 值更新当前节点的 low 值
                low[u] = std::min(low[u], low[v]);
            } else if (in_stack[v]) { //返祖边, 如果 v 在栈中，说明构成了环
                low[u] = std::min(low[u], dfn[v]);
            }
        }

        // 如果 dfn == low，说明找到了一个 SCC 的起始点
        if (low[u] == dfn[u]) {
            scc_cnt++;
            while (1) {
                int v = st.top(); st.pop();
                in_stack[v] = 0;
                scc_id[v] = scc_cnt; // 标记所属 SCC 编号
                if (v == u) break; // 直到找到起始点
            }
        }
    }

    void solve() {
        for (int i = 1; i <= n; i++) {
            if (!dfn[i]) dfs(i);
        }
    }
};