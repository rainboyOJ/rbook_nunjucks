/**
 * Author by {{author}} blog: {{blog}} github : {{github}}
 * rbook: -> https://rbook.roj.ac.cn  https://rbook2.roj.ac.cn
 * date: {{date}}
 */

#include <bits/stdc++.h>
using namespace std;
const int maxn = 1e6+5; // 点
const int maxe = 2e6+5; // 边 (注意：要是题目边数的2倍)
const long long INF = 1e18;

int n,m;
int s,t; // 源点 汇点
int a[maxn];

// 存图的模板
{{include "code/graph/linklist.cpp"}}

// Dinic算法最大流模板 - 基于linkList存图
struct Dinic {
    vector<int> level, cur;  // level: BFS分层, cur: 当前弧优化
    int n;  // 节点数
    
    void init(int n) {
        // 重置linkList
        e.edge_cnt = 0;
        memset(e.h, -1, sizeof(e.h));

        level.resize(n+5);
        cur.resize(n+5);
        this->n = n;
    }
    
    // 添加边：从u到v，容量为cap
    // 使用技巧：正向边和反向边的索引相差1，通过异或1来找到对应边
    void addEdge(int u, int v, int cap) {
        e.add(u, v, cap);    // 正向边，w字段存储容量
        e.add(v, u, 0);      // 反向边，容量为0
    }
    
    // BFS分层，构建层次图
    bool bfs(int s, int t) {
        fill(level.begin(), level.end(), -1);
        queue<int> q;
        level[s] = 0;
        q.push(s);
        
        while (!q.empty()) {
            int u = q.front();
            q.pop();
            
            // 使用linkList的遍历方式
            for(int i = e.h[u] ; ~i ;i = e[i].next) {
                int v = e[i].v, cap = e[i].w;
                if (cap > 0 && level[v] < 0) {
                    level[v] = level[u] + 1;
                    q.push(v);
                }
            }
        }
        
        return level[t] >= 0;  // 返回是否能到达汇点
    }
    
    // DFS寻找增广路
    // 到达点u流量为preFlow
    // 计算从点u出发的最大流，到达点t
    // 本质是一个DAG 上的dp
    long long dfs(int u, int t, long long preFlow) {
        if (u == t || preFlow == 0) return preFlow;
        long long flow = 0;
        
        // 当前弧优化：从cur[u]开始遍历
        for (int& cid = cur[u]; cid != -1; cid = e[cid].next) {
            auto& edge = e[cid]; // 当前弧优化
            int to = edge.v;
            long long cap = edge.w;
            
            if (level[u] + 1 != level[to] || cap <= 0) continue;
            
            long long tr = dfs(to, t, min(preFlow, cap));

            // 更新容量
            e[cid].w -= tr ;     // 正向边容量减少
            e[cid ^ 1].w += tr; // 反向边容量增加（利用异或找到反向边）
            flow += tr;
            preFlow -= tr;
            if (preFlow == 0) break;
        }
        
        // 炸点优化
        // 剪枝：取掉增广完毕的点
        if (flow == 0) level[u] = -1;
        return flow;
    }
    
    // 求从s到t的最大流
    long long maxFlow(int s, int t) {
        long long flow = 0;
        while (bfs(s, t)) {  // 能够分层
            // 当前弧优化重置：将cur设置为每个节点的第一条边
            for (int i = 0; i <= n; i++) {
                cur[i] = e.h[i];  // 使用linkList的operator()获取head[i]
            }
            
            // 多路增广
            flow += dfs(s, t, LLONG_MAX);
        }
        return flow;
    }
} dinic;

void init(){
    std::cin >> n >> m;
    std::cin >> s >> t;

    dinic.init(n);
    
    for (int i = 0; i < m; i++) {
        int u, v,cap;
        cin >> u >> v >> cap;
        
        //添加流量,自动添加反向边
        dinic.addEdge(u, v, cap); 
    }

}

// 使用示例：
int main() {
    ios::sync_with_stdio(0); cin.tie(0);
    init();
    
    cout << dinic.maxFlow(s,t) << "\n";
    
    return 0;
}

/*
复杂度分析：
- 时间复杂度：O(V²E) 一般情况下表现很好，对于单位容量网络是O(min(V^(2/3), E^(1/2)) * E)
- 空间复杂度：O(V + E)

使用说明：
1. 创建Dinic实例：Dinic dinic(n);
2. 添加边：dinic.addEdge(u, v, cap);
3. 求最大流：long long flow = dinic.maxFlow(source, sink);

注意事项：
- 节点编号从0开始
- 如果题目给的是1-indexed，记得转换
- 容量使用long long防止溢出
- 无向边需要添加两条有向边
*/
