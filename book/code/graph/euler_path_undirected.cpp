#include <bits/stdc++.h>
using namespace std;

// 无向图欧拉路 / 欧拉回路（Hierholzer 算法）
// 输入：
//   n m
//   m 行 u v
// 输出：
//   一条欧拉路（或欧拉回路）的顶点序列，不存在时输出 NO
//
// 代码分层：
//   1. 建图层：init / add_edge
//   2. 判定层：connected（连通性）+ check_degree（度数条件）
//   3. 构造层：hierholzer（算法本体，只管在有解时走出路径）
//   4. 入口层：solve（把判定和串起来）+ main（输入输出）
//
// 存在性条件（两个都要满足）：
//   1. 所有非孤立点连通；
//   2. 奇度数点个数为 0（欧拉回路）或 2（欧拉路，必须从奇度数点出发）。
// 时间复杂度 O(n + m)。

struct EulerPathUndirected {
    struct Edge {
        int to;
        int id; // 边的编号，用来标记这条边是否已经走过（重边靠编号区分）
    };

    int n = 0;
    int edge_cnt = 0; // 已加入的边数
    vector<vector<Edge>> g;
    vector<int> deg; // 每个点的度数

    void init(int _n) {
        n = _n;
        edge_cnt = 0;
        g.assign(n + 1, vector<Edge>());
        deg.assign(n + 1, 0);
    }

    // 加一条无向边 u - v。两个方向共用同一个 id。
    void add_edge(int u, int v) {
        ++edge_cnt;
        g[u].push_back({v, edge_cnt});
        g[v].push_back({u, edge_cnt});
        if (u == v) {
            deg[u] += 2; // 易错点：自环给度数贡献 2
        } else {
            ++deg[u];
            ++deg[v];
        }
    }

    // ---- 判定层 ----

    // 条件 1：所有非孤立点是否连通。
    // 孤立点不参与判断：它们既不影响欧拉路的存在性，也不会出现在路径里。
    bool connected() {
        // 找任意一个「有边相连」的点作为 DFS 起点
        int first = -1;
        for (int i = 1; i <= n; ++i) {
            if (deg[i] > 0) {
                first = i;
                break;
            }
        }
        // 边界：图中没有任何边，视为连通（此时欧拉路是空序列）
        if (first == -1) return true;

        // 迭代 DFS 检查连通性（递归写法在链状图上可能爆栈）。
        // 自环 u == v 时 g[u] 里有 {u, id}，vis 会直接跳过，不影响结果。
        vector<char> vis(n + 1, 0);
        stack<int> st;
        st.push(first);
        vis[first] = 1;
        while (!st.empty()) {
            int u = st.top();
            st.pop();
            for (const auto &e : g[u]) {
                if (!vis[e.to]) {
                    vis[e.to] = 1;
                    st.push(e.to);
                }
            }
        }

        for (int i = 1; i <= n; ++i) {
            if (deg[i] > 0 && !vis[i]) return false;
        }
        return true;
    }

    // 条件 2：奇度数点只能有 0 个或 2 个。
    // 有 2 个奇点时必须从奇点出发（欧拉路），全偶度时从任意有边的点出发（欧拉回路）。
    // 判定通过时 start 是合法起点。
    bool check_degree(int &start) {
        start = -1;
        for (int i = 1; i <= n; ++i) {
            if (deg[i] > 0) {
                start = i; // 回路情形的默认起点
                break;
            }
        }

        int odd_count = 0;
        for (int i = 1; i <= n; ++i) {
            if (deg[i] % 2 == 1) {
                ++odd_count;
                start = i; // 最后一个奇点；恰有 2 个奇点时任取一个作起点都合法
            }
        }
        return odd_count == 0 || odd_count == 2;
    }

    // ---- 构造层 ----

    // Hierholzer 算法本体：从 start 出发走出一条用完所有边的路径。
    // 前提：connected() 与 check_degree() 都已通过。
    // 返回顶点序列，正常时长度为边数 + 1。
    vector<int> hierholzer(int start) {
        vector<char> used(edge_cnt + 1, 0); // used[id]：第 id 条边是否已走
        // iter[u]：u 的「当前弧」，指向第一条还没走过的邻接边。
        // 走过的边不再扫描，保证每条边只被处理一次，总复杂度 O(n + m)。
        // 易错点：不要每次从 g[u][0] 重新扫，否则退化到 O(m^2)。
        // 无向边两个方向共用一个 id，标记一次就表示这条边整体已用，
        // 所以不会把同一条边来回走两遍。
        vector<int> iter(n + 1, 0), stack_path, path;
        stack_path.push_back(start);

        while (!stack_path.empty()) {
            int u = stack_path.back();
            // 跳过已经走过的边（重边可能出现连续多条已用边）
            while (iter[u] < (int)g[u].size() && used[g[u][iter[u]].id]) {
                ++iter[u];
            }

            if (iter[u] == (int)g[u].size()) {
                // u 的边走完了：u 是当前这条「死胡同」的端点，
                // 此时才把 u 记入 path（后序收集，最终 reverse 后才是正序）。
                path.push_back(u);
                stack_path.pop_back();
            } else {
                Edge e = g[u][iter[u]++]; // 取走这条边
                used[e.id] = 1;
                stack_path.push_back(e.to);
            }
        }

        // path 是「回溯顺序」收集的，reverse 后才是从起点到终点的欧拉路。
        reverse(path.begin(), path.end());
        return path;
    }

    // ---- 入口层 ----

    // 总入口：存在欧拉路/回路时把顶点序列写进 path 并返回 true。
    // 图中没有边时 path 是空序列，也视为有解。
    bool solve(vector<int> &path) {
        path.clear();
        int start = -1;
        if (!connected() || !check_degree(start)) return false;
        if (edge_cnt == 0) return true; // 边界：没有任何边，欧拉路是空序列

        path = hierholzer(start);
        // 理论上前面的判定已经保证有解，这里再兜底一次防御非法输入。
        if ((int)path.size() != edge_cnt + 1) {
            path.clear();
            return false;
        }
        return true;
    }
};

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n, m;
    if (!(cin >> n >> m)) return 0;

    EulerPathUndirected solver;
    solver.init(n);
    for (int i = 0; i < m; ++i) {
        int u, v;
        cin >> u >> v;
        solver.add_edge(u, v);
    }

    vector<int> path;
    if (!solver.solve(path)) {
        cout << "NO\n";
        return 0;
    }
    for (int i = 0; i < (int)path.size(); ++i) {
        if (i) cout << ' ';
        cout << path[i];
    }
    cout << '\n';
    return 0;
}
