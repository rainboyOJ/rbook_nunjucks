/**
 * Author by Rainboy blog: https://blog.roj.ac.cn github : https://github.com/rainboylvx
 * Annotated by Gemini for educational purposes
 */
#include <bits/stdc++.h>
using namespace std;
typedef  long long ll;
typedef  unsigned long long ull;

// 常量定义
const int maxn = 205;      // 最大点数 (根据题目数据范围调整)
const int maxe = 5005 * 2; // 最大边数：注意要乘以2，因为每一条有向边都需要一条反向边
const ll INF = 1e18;       // 无穷大，用于初始化流量
int n, m;                  // n:点数, m:边数
int s, t;                  // s:源点, t:汇点
int a[maxn];               // (此数组在代码中未被使用，可以移除)

// 链式前向星：存图结构
struct linkList {
    // u:起点(可选), v:终点, w:剩余容量(权值), next:下一条同起点的边
    typedef struct {int u, v, w, next;} edge; 
    edge e[maxe];
    int h[maxn], edge_cnt = 0; // h[u]: 点u的第一条边的下标

    // 构造函数：初始化头指针数组为 -1
    linkList(){
        edge_cnt = 0;
        memset(h, -1, sizeof(h));
    }

    // 遍历点 u 的辅助函数 (当前代码中未使用，但写库时常用)
    template<typename U>
    void for_each(int u, U func){
        for(int i = h[u]; i != -1; i = e[i].next)
            func(e[i].u, e[i].v, e[i].w);
    }

    // 核心加边函数：单向边
    void add(int u, int v, int w=0){
        // e[edge_cnt] 存储边的信息
        // h[u] 存储的是上一条以 u 为起点的边的下标
        // 也就是将当前边插入到链表的头部
        e[edge_cnt] = {u, v, w, h[u]};
        h[u] = edge_cnt++; // 更新头指针，edge_cnt 后移
    }

    // 添加双向边 (通常用于无向图，网络流一般不用这个，而是手动加反向边)
    void add2(int u, int v, int w=0){
        add(u, v, w);
        add(v, u, w);
    }

    // 重载 [] 方便直接像数组一样访问边 e[i]
    edge& operator[](int i){ return e[i]; }
    // 重载 () 方便获取头指针 h[u]
    int operator()(int u){ return h[u]; }
} e;

// 全局辅助数组
int pre[maxn]; // 记录增广路径：pre[v] 存储的是“通向 v 的那条边的下标 (edge index)”
ll flow[maxn]; // flow[v] 记录从源点 s 到 v 的路径上的“瓶颈容量”（当前路径能流过的最大流量）

// BFS 寻找增广路 (Augmenting Path)
// 返回值：true 表示找到了从 s 到 t 的路径，false 表示没路了（算法结束）
bool bfs() {
    // 初始化 pre 数组为 -1，表示所有点未被访问
    memset(pre, -1, sizeof(pre));
    
    queue<int> q;
    q.push(s);
    flow[s] = INF; // 源点的流量视为无限大
    pre[s]  = 0;   // 源点不需要前驱边 (这里设为 0 或任意非 -1 值防止重复访问)

    while ( !q.empty() ) {
        int u = q.front();
        q.pop();

        // 优化：如果已经搜到了汇点，说明找到了一条路，可以直接返回 true
        if( u == t) return true;

        // 遍历 u 的所有出边
        for(int i = e.h[u]; i != -1; i = e[i].next)
        {
            int v = e[i].v; // 边的终点
            int w = e[i].w; // 边的剩余容量

            // 两个条件：
            // 1. pre[v] == -1 : 点 v 没有被访问过（保证是最短路/不走回头路）
            // 2. w > 0        : 这条边还有剩余容量，可以流过水
            if( pre[v] == -1 && w > 0)
            {
                pre[v] = i; // 【关键】记录是从哪条边(i) 走到 v 的，用于稍后倒推路径

                // 更新路径上的瓶颈流量：取“当前流过来的量”和“这条边容量”的较小值
                flow[v] = std::min(flow[u], (ll)w);
                
                q.push(v);
            }
        }
    }

    // 队列空了也没碰到 t，说明图不连通或者容量已满
    return false;
}

// Edmonds-Karp 算法主函数
ll EK() {
    ll max_flow = 0; // 总最大流

    // 只要能在残留网络中找到增广路 (s -> t)，就一直循环
    while ( bfs() ) {

        // 本次增广路能增加的流量，就是汇点 t 处的瓶颈流量
        ll increment = flow[t];
        max_flow += increment;

        // 从汇点 t 沿着 pre 数组回溯到源点 s，更新边的容量
        int v = t;
        while(v != s) {
            int i = pre[v]; // 获取通向 v 的那条边的下标

            // 1. 正向边容量减少 (流过去了)
            e[i].w -= increment;

            // 2. 反向边容量增加 (允许反悔)
            // 技巧：i^1 
            // 因为加边是一对一对加的：0和1是一对，2和3是一对...
            // 偶数 x 的 x^1 是 x+1，奇数 y 的 y^1 是 y-1
            // 这样就能快速找到对应的反向边
            e[i^1].w += increment;

            // 移动到上一个点
            // e[i^1].v 是反向边的终点，也就是正向边的起点 u
            v = e[i^1].v; 
        }
    }

    return max_flow;
}

void init(){
    std::cin >> n >> m >> s >> t;
    for(int i = 1; i <= m; ++i) 
    {
        int u, v, w;
        std::cin >> u >> v >> w;
        
        // 【关键构建】
        // 正向边：容量为 w
        e.add(u, v, w); 
        // 反向边：初始容量为 0 (建立残留网络的基础)
        e.add(v, u, 0); 
    }
}

signed main () {
    // 关闭同步，加速 IO
    ios::sync_with_stdio(false); cin.tie(0);
    
    init();
    std::cout << EK() << "\n";
    
    return 0;
}