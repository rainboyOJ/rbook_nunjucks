// 定义边的结构体
struct Edge {
    int u, v;
    long long w;
    // 重载 < 运算符，方便 std::sort 直接排序
    bool operator<(const Edge& other) const {
        return w < other.w;
    }
};

// Kruskal 算法封装结构体
struct KruskalAlgorithm {
    struct DSU {
        std::vector<int> fa;
        // 初始化并查集
        void init(int n) {
            fa.resize(n + 1);
            std::iota(fa.begin(), fa.end(), 0); // 赋值 0, 1, 2... n
        }
        int find(int x) {
            return x == fa[x] ? x : fa[x] = find(fa[x]); // 路径压缩
        }
        bool merge(int x, int y) {
            int fx = find(x), fy = find(y);
            if (fx == fy) return false; // 已经在同一个集合
            fa[fx] = fy;
            return true;
        }
    } dsu;

    int n; // 点的数量
    std::vector<Edge> edges; // 存储所有边

    // 初始化：清空边，设定点数
    void init(int _n) {
        n = _n;
        edges.clear();
        dsu.init(n);
    }

    // 加边函数：心智负担低，不用操心数组下标
    void add_edge(int u, int v, long long w) {
        edges.push_back({u, v, w});
    }

    // 执行算法
    // 返回值：最小生成树的权值和。如果无法连通，返回 -1
    long long solve() {
        // 1. 排序：贪心的基础
        std::sort(edges.begin(), edges.end());
        
        // 2. 重新初始化并查集状态（防止多次调用 solve 出错）
        dsu.init(n);

        long long ans = 0; // 权值和，注意开 long long
        int cnt = 0;       // 记录选了多少条边

        for (const auto& e : edges) {
            if (dsu.merge(e.u, e.v)) { // 如果不在同一集合，合并
                ans += e.w;
                cnt++;
                // 如果需要输出选中的边，可以在这里打印
                // cout << e.u << " " << e.v << " " << e.w << endl;
            }
            if (cnt == n - 1) break; // 优化：选够了边就退出
        }

        // 3. 连通性检查：如果选的边数少于 n-1，说明图不连通
        if (cnt < n - 1) return -1;
        
        return ans;
    }
} kruskal;
