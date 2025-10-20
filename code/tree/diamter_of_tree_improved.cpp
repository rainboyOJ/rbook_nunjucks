/**
 * 求树上最长的距离：直径（改进版本）
 * 适用于Codeforces竞赛编程
 */
#include <bits/stdc++.h>
using namespace std;

const int maxn = 200005;  // 根据题目需求调整

// 边结构
struct Edge {
    int to, next, w;
} e[maxn << 1];

int head[maxn], cnt;

void add_edge(int u, int v, int w = 1) {
    e[++cnt] = {v, head[u], w};
    head[u] = cnt;
    e[++cnt] = {u, head[v], w};
    head[v] = cnt;
}

template<int N = maxn>
struct TreeDiameter {
    int dis[N];        // 距离数组
    int parent[N];     // 父节点数组，用于记录路径
    int farthest[N];   // 远端点数组
    int st, ed;        // 直径的端点
    
    // 重置数组
    void reset() {
        fill(dis, dis + N, 0);
        fill(parent, parent + N, -1);
    }
    
    // DFS求最远点
    int dfs(int u, int fa, int &max_dist, int &far_node) {
        dis[u] = 0;
        farthest[u] = u;
        
        for (int i = head[u]; i; i = e[i].next) {
            int v = e[i].to;
            if (v == fa) continue;
            
            dfs(v, u, max_dist, far_node);
            int len = dis[v] + e[i].w;
            
            if (len > dis[u]) {
                dis[u] = len;
                farthest[u] = farthest[v];
                parent[u] = v;
            }
        }
        
        if (dis[u] > max_dist) {
            max_dist = dis[u];
            far_node = farthest[u];
        }
        
        return farthest[u];
    }
    
    // 两遍DFS求直径
    int two_dfs(int start_node = 1) {
        reset();
        
        // 第一遍：从起始点找最远点
        int max_dist = 0, far_node = start_node;
        dfs(start_node, 0, max_dist, far_node);
        st = far_node;
        
        // 第二遍：从st点找直径
        reset();
        max_dist = 0;
        dfs(st, 0, max_dist, far_node);
        ed = far_node;
        
        return max_dist;  // 返回直径长度
    }
    
    // BFS版本（适用于边权为1的树）
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
            
            for (int i = head[u]; i; i = e[i].next) {
                int v = e[i].to;
                if (v == parent[u]) continue;
                
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
    
    // 使用BFS求直径
    int get_diameter_bfs(int start_node = 1) {
        st = bfs(start_node);
        ed = bfs(st);
        return dis[ed];
    }
    
    // 获取直径路径
    vector<int> get_diameter_path() {
        vector<int> path;
        int u = ed;
        while (u != -1) {
            path.push_back(u);
            u = parent[u];
        }
        reverse(path.begin(), path.end());
        return path;
    }
};

// 使用示例
int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    
    int n;
    cin >> n;
    
    // 重置邻接表
    memset(head, 0, sizeof(head));
    cnt = 0;
    
    TreeDiameter<> solver;
    
    for (int i = 1; i < n; i++) {
        int u, v;
        cin >> u >> v;
        add_edge(u, v);
    }
    
    int diameter = solver.two_dfs(1);
    cout << diameter << "\n";
    
    return 0;
}