// 假设外部已经有了前向星/邻接表结构体 e 和 head 数组
// struct Edge { int v, next; } e[maxe]; int h[maxn];

struct TarjanEBCC {
    int n, timer;
    std::stack<int> st;
    int dfn[maxn], low[maxn];
    int bcc_cnt;         // e-BCC 计数
    int bcc_id[maxn];    // 记录每个点属于哪个 e-BCC (1 ~ bcc_cnt)
    
    // 初始化
    void set(int _n) {
        n = _n;
        timer = bcc_cnt = 0;
        memset(dfn, 0, sizeof(dfn));
        memset(low, 0, sizeof(low));
        memset(bcc_id, 0, sizeof(bcc_id));
        while (!st.empty()) st.pop();
    }

    // e-BCC 的 DFS
    // 参数 fa: 父节点，用于防止直接沿反向边回去 (处理无向图)
    void dfs(int u, int fa) {
        dfn[u] = low[u] = ++timer;
        st.push(u);

        for (int i = e.h[u]; ~i; i = e[i].next) {
            int v = e[i].v;
            
            if (v == fa) continue; // 无向图核心：不走回头路 (若有重边需改用边下标判断)

            if (!dfn[v]) {
                dfs(v, u);
                low[u] = std::min(low[u], low[v]);
            } else {
                low[u] = std::min(low[u], dfn[v]);
            }
        }

        // 核心区别：当 low[u] == dfn[u] 时，说明 u 是该连通分量的“根”
        // 此时栈中 u 及以上的点构成一个完整的 e-BCC
        if (low[u] == dfn[u]) {
            bcc_cnt++;
            while (true) {
                int node = st.top(); 
                st.pop();
                bcc_id[node] = bcc_cnt; // 标记该点属于哪个分量
                if (node == u) break; // 把该分量的点都弹出了
            }
        }
    }

    // 入口函数
    void solve() {
        for (int i = 1; i <= n; i++) {
            if (!dfn[i]) {
                dfs(i, 0); 
            }
        }
    }
};