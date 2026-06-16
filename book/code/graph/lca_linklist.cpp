// 需要使用linklist e;
const int MAXLOG = 20;   
struct LCA {
    // f[u][i] 表示 u 的 2^i 祖先
    // d[u] 表示 u 的深度
    int f[maxn][MAXLOG + 1];
    int d[maxn];
    
    // 邻接表引用，避免全局依赖，初始化时传入
    // linklist *e;

    // 初始化函数
    // n: 节点数量
    // root: 根节点
    // void init(int n, int root,linlist & E);
        // *e = &E;
    void init(int n, int root){
        
        // 预处理深度和父节点
        dfs(root, 0, 1);
        
        // 倍增预处理：类似DP
        // f[u][j] = f[ f[u][j-1] ][ j-1 ]
        // 意思：u跳2^j步 = u先跳2^(j-1)步，再从那里跳2^(j-1)步
        for (int j = 1; j <= MAXLOG; ++j) {
            for (int i = 1; i <= n; ++i) {
                
                // 不需要判断中间点是否存在,如果中间的点不存在
                // 那么f[i][j] = 0 变成黑洞点
                f[i][j] = f[ f[i][j-1] ][ j-1 ];
              
            }
        }
    }

    // DFS 建树
    void dfs(int u, int fa, int depth) {
        d[u] = depth;
        f[u][0] = fa; // 2^0 = 1，直接父亲
        
        for (int i = e.h[u]; i!= -1;i = e[i].next) 
            if (e[i].v != fa) 
                dfs(e[i].v, u, depth + 1);

    }

    // 查询 u 和 v 的最近公共祖先
    int ask(int u, int v) {
        // 1. 保证 u 是深度较大的那个（在下面）
        if (d[u] < d[v]) swap(u, v);

        // 2. 将 u 上跳，直到和 v 同一层
        // i 从大到小试，类似二进制拆分
        for (int i = MAXLOG; i >= 0; --i) {
            // 如果 u 跳 2^i 步之后，深度依然 >= v 的深度，就跳
            if (d[u] - (1 << i) >= d[v]) {
                u = f[u][i];
            }
        }

        // 3. 如果跳到了同一层，且 u == v，说明 v 就是 u 的祖先
        if (u == v) return u;

        // 4. u 和 v 一起往上跳，直到 LCA 的下一层
        for (int i = MAXLOG; i >= 0; --i) {
            // 如果跳上去不一样，说明还没遇到 LCA，继续跳
            if (f[u][i] != f[v][i]) {
                u = f[u][i];
                v = f[v][i];
            }
        }

        // 5. 此时 u 和 v 的父亲就是 LCA
        return f[u][0];
    }
} lca;

