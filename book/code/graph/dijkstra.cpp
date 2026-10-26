#include <algorithm>
#include <functional>
#include <queue>
#include <utility>
#include <vector>
using namespace std;

typedef long long ll;
typedef pair<ll, int> State;  // (dis, u)：距离在前，小根堆才能先弹最小距离

const int maxn = 100000 + 5;         // 点数上限，按题面规模调整
const ll INF = (1LL << 62);          // 约 4.6e18：比任何合法最短距离都大

struct Edge {
    int to;  // 终点
    ll w;    // 边权，必须非负
};

vector<Edge> g[maxn];  // g[u]：从 u 出发的全部边
ll dis[maxn];          // dis[i]：s 到 i 的当前最短距离，INF 表示暂时不可达

// 点编号 1..n；换一张图前调用，避免多组数据残留旧边。
void init_graph(int n) {
    for (int u = 1; u <= n; ++u) g[u].clear();
}

// 加一条有向边 u -> v，权 w；无向边调用两次。
void add_edge(int u, int v, ll w) {
    g[u].push_back({v, w});
}

// 求 s 到 1..n 的单源最短路，结果写在全局 dis 里。
// 要求所有边权非负，且 n < maxn、合法的有限最短距离严格小于 INF。
void dijkstra(int n, int s) {
    fill(dis + 1, dis + n + 1, INF);  // 只清 1..n，dis[0] 不用

    // priority_queue 默认是大根堆，用 greater 反转成小根堆
    priority_queue<State, vector<State>, greater<State>> q;
    dis[s] = 0;
    q.push({0, s});

    while (!q.empty()) {
        ll du = q.top().first;  // 当前最小的候选距离
        int u = q.top().second;  // 候选距离对应的点
        q.pop();

        if (du != dis[u]) continue;  // 过期记录：u 已有更短距离，不再扫它的出边

        for (const Edge &e : g[u]) {
            // du < INF 且 w >= 0，仍先判结果再相加，避免大边权相加溢出
            if (e.w >= INF - du) continue;
            ll nd = du + e.w;
            if (nd >= dis[e.to]) continue;  // 只接受严格改善，零权环不会反复入堆
            dis[e.to] = nd;
            q.push({nd, e.to});
        }
    }
}
