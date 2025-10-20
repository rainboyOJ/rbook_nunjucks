/**
 * 树直径算法完整版本
 * 包含：DFS、BFS、Dijkstra三种实现
 * 适用于不同边权情况
 */
#include <bits/stdc++.h>
using namespace std;

const int maxn = 200005;
const long long INF = 1e18;

// 边结构（支持任意权重）
struct Edge {
    int to, next;
    long long w;
    Edge(int t = 0, int n = 0, long long w = 0) : to(t), next(n), w(w) {}
} e[maxn << 1];

int head[maxn], cnt;
int n;

void add_edge(int u, int v, long long w = 1) {
    e[++cnt] = Edge(v, head[u], w);
    head[u] = cnt;
    e[++cnt] = Edge(u, head[v], w);
    head[v] = cnt;
}

template<int N = maxn>
struct TreeDiameterAdvanced {
    long long dis[N];        // 距离数组
    int parent[N];          // 父节点数组
    bool visited[N];        // 访问标记
    int st, ed;             // 直径端点
    long long diameter_len; // 直径长度
    
    // 重置数组
    void reset() {
        fill(dis, dis + N, INF);
        fill(parent, parent + N, -1);
        fill(visited, visited + N, false);
    }
    
    // ==================== DFS版本（加权树）====================
    int dfs(int u, int fa) {
        dis[u] = 0;
        parent[u] = fa;
        int farthest_node = u;
        
        for (int i = head[u]; i; i = e[i].next) {
            int v = e[i].to;
            if (v == fa) continue;
            
            int leaf = dfs(v, u);
            long long len = dis[v] + e[i].w;
            
            if (len > dis[u]) {
                dis[u] = len;
                farthest_node = leaf;
            }
        }
        
        return farthest_node;
    }
    
    // 两遍DFS求直径（支持加权树）
    long long get_diameter_dfs(int start_node = 1) {
        reset();
        st = dfs(start_node, 0);      // 第一遍找端点st
        
        reset();
        ed = dfs(st, 0);              // 第二遍从st找ed
        diameter_len = dis[ed];
        
        return diameter_len;
    }
    
    // ==================== BFS版本（仅限边权=1）====================
    int bfs(int start) {
        reset();
        queue<int> q;
        q.push(start);
        dis[start] = 0;
        parent[start] = -1;
        
        int farthest_node = start;
        
        while (!q.empty()) {
            int u = q.front();
            q.pop();
            visited[u] = true;
            
            for (int i = head[u]; i; i = e[i].next) {
                int v = e[i].to;
                if (visited[v]) continue;
                
                dis[v] = dis[u] + e[i].w;
                parent[v] = u;
                q.push(v);
                
                if (dis[v] > dis[farthest_node]) {
                    farthest_node = v;
                }
            }
        }
        
        return farthest_node;
    }
    
    // BFS求直径（仅限边权为1）
    long long get_diameter_bfs(int start_node = 1) {
        st = bfs(start_node);
        ed = bfs(st);
        diameter_len = dis[ed];
        return diameter_len;
    }
    
    // ==================== Dijkstra版本（支持任意权重）====================
    void dijkstra(int start) {
        reset();
        priority_queue<pair<long long, int>, vector<pair<long long, int>>, greater<>> pq;
        dis[start] = 0;
        pq.push({0, start});
        
        while (!pq.empty()) {
            auto [d, u] = pq.top();
            pq.pop();
            
            if (d != dis[u]) continue;
            
            for (int i = head[u]; i; i = e[i].next) {
                int v = e[i].to;
                if (dis[v] > d + e[i].w) {
                    dis[v] = d + e[i].w;
                    parent[v] = u;
                    pq.push({dis[v], v});
                }
            }
        }
    }
    
    // 两遍Dijkstra求直径（支持任意权重）
    long long get_diameter_dijkstra(int start_node = 1) {
        // 第一遍：从起点找最远点
        dijkstra(start_node);
        st = max_element(dis + 1, dis + n + 1) - dis;
        
        // 第二遍：从st点求直径
        dijkstra(st);
        ed = max_element(dis + 1, dis + n + 1) - dis;
        diameter_len = dis[ed];
        
        return diameter_len;
    }
    
    // ==================== 路径和辅助函数 ====================
    // 获取直径路径
    vector<int> get_diameter_path() {
        vector<int> path;
        int u = ed;
        while (u != -1 && u != 0) {
            path.push_back(u);
            u = parent[u];
        }
        reverse(path.begin(), path.end());
        return path;
    }
    
    // 获取所有节点到直径的最小距离
    vector<long long> get_min_dist_to_diameter() {
        vector<int> diameter_path = get_diameter_path();
        vector<long long> min_dist(n + 1, INF);
        
        // 预处理直径上点到其他点的距离
        for (int u : diameter_path) {
            reset();
            dijkstra(u);
            for (int i = 1; i <= n; i++) {
                min_dist[i] = min(min_dist[i], dis[i]);
            }
        }
        
        return min_dist;
    }
    
    // 智能选择算法
    long long get_diameter(int start_node = 1) {
        // 检查边权是否都是1
        bool all_weight_one = true;
        for (int i = 1; i <= cnt; i++) {
            if (e[i].w != 1) {
                all_weight_one = false;
                break;
            }
        }
        
        if (all_weight_one && n <= 100000) {
            // 小规模+边权为1，用DFS更快
            return get_diameter_dfs(start_node);
        } else if (all_weight_one) {
            // 大规模+边权为1，避免递归深度问题
            return get_diameter_bfs(start_node);
        } else {
            // 有加权边，使用Dijkstra
            return get_diameter_dijkstra(start_node);
        }
    }
};

// ==================== 测试和示例 ====================
void test_different_weights() {
    cout << "===== 测试不同边权情况 =====\n";
    
    // 重置图
    memset(head, 0, sizeof(head));
    cnt = 0;
    
    // 例子：加权树
    // 1-2(3), 2-3(2), 2-4(4), 3-5(1), 4-6(2)
    vector<tuple<int, int, long long>> edges = {
        {1, 2, 3}, {2, 3, 2}, {2, 4, 4}, {3, 5, 1}, {4, 6, 2}
    };
    
    n = 6;
    for (auto [u, v, w] : edges) {
        add_edge(u, v, w);
    }
    
    TreeDiameterAdvanced<> solver;
    
    // 测试DFS
    long long dfs_result = solver.get_diameter_dfs(1);
    cout << "DFS版本直径长度: " << dfs_result << "\n";
    auto dfs_path = solver.get_diameter_path();
    cout << "DFS路径: ";
    for (int x : dfs_path) cout << x << " ";
    cout << "\n";
    
    // 测试Dijkstra
    long long dijk_result = solver.get_diameter_dijkstra(1);
    cout << "Dijkstra版本直径长度: " << dijk_result << "\n";
    auto dijk_path = solver.get_diameter_path();
    cout << "Dijkstra路径: ";
    for (int x : dijk_path) cout << x << " ";
    cout << "\n";
    
    // 智能选择
    long long smart_result = solver.get_diameter(1);
    cout << "智能选择结果: " << smart_result << "\n\n";
}

void test_unweighted_tree() {
    cout << "===== 测试无权树情况 =====\n";
    
    // 重置图
    memset(head, 0, sizeof(head));
    cnt = 0;
    
    // 例子：无权树
    // 1-2, 2-3, 2-4, 3-5, 4-6
    vector<pair<int, int>> edges = {
        {1, 2}, {2, 3}, {2, 4}, {3, 5}, {4, 6}
    };
    
    n = 6;
    for (auto [u, v] : edges) {
        add_edge(u, v, 1);
    }
    
    TreeDiameterAdvanced<> solver;
    
    // BFS版本
    long long bfs_result = solver.get_diameter_bfs(1);
    cout << "BFS版本直径长度: " << bfs_result << "\n";
    auto bfs_path = solver.get_diameter_path();
    cout << "BFS路径: ";
    for (int x : bfs_path) cout << x << " ";
    cout << "\n";
    
    // DFS版本
    long long dfs_result = solver.get_diameter_dfs(1);
    cout << "DFS版本直径长度: " << dfs_result << "\n";
    
    // Dijkstra版本
    long long dijk_result = solver.get_diameter_dijkstra(1);
    cout << "Dijkstra版本直径长度: " << dijk_result << "\n";
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    
    // 运行测试
    test_different_weights();
    test_unweighted_tree();
    
    cout << "===== 根据边权智能选择算法的建议 =====\n";
    cout << "1. 边权都为1 & n<=1e5: 使用DFS（速度最快）\n";
    cout << "2. 边权都为1 & n>1e5: 使用BFS（避免栈溢出）\n";
    cout << "3. 有加权边: 使用Dijkstra（保证正确性）\n";
    cout << "4. 不确定情况: 使用智能选择方法 get_diameter()\n\n";
    
    return 0;
}