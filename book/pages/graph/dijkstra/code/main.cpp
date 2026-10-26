#include <algorithm>
#include <functional>
#include <iostream>
#include <queue>
#include <utility>
#include <vector>
using namespace std;

typedef long long ll;
typedef pair<ll, int> State;  // (dis, u)：距离在前，小根堆才能先弹最小距离

const int maxn = 100000 + 5;  // 点数上限，按题面规模调整
const ll INF = (1LL << 62);   // 约 4.6e18：比任何合法最短距离都大

struct Edge {
    int to;  // 终点
    ll w;    // 边权，必须非负
};

vector<Edge> g[maxn];  // g[u]：从 u 出发的全部边
ll dis[maxn];          // dis[i]：s 到 i 的当前最短距离
int n;                 // 点数
int m;                 // 边数
int s;                 // 源点

// 加一条有向边 u -> v，权 w；无向边调用两次。
void add_edge(int u, int v, ll w) {
    g[u].push_back({v, w});
}

void read_input() {
    cin >> n >> m >> s;
    for (int i = 0; i < m; i++) {
        int u, v;
        ll w;
        cin >> u >> v >> w;
        add_edge(u, v, w);
    }
}

// 求 s 到 1..n 的单源最短路，结果写在全局 dis 里。
void dijkstra(int s) {
    fill(dis + 1, dis + n + 1, INF);

    // priority_queue 默认是大根堆，用 greater 反转成小根堆
    priority_queue<State, vector<State>, greater<State>> q;
    dis[s] = 0;
    q.push({0, s});

    while (!q.empty()) {
        ll du = q.top().first;   // 当前最小的候选距离
        int u = q.top().second;  // 候选距离对应的点
        q.pop();

        if (du != dis[u]) continue;  // 过期记录：u 已有更短距离，不再扫它的出边

        for (const Edge &e : g[u]) {
            // du < INF 且 w >= 0，仍先判结果再相加，避免大边权相加溢出
            if (e.w >= INF - du) continue;
            ll nd = du + e.w;
            if (nd >= dis[e.to]) continue;  // 只接受严格改善
            dis[e.to] = nd;
            q.push({nd, e.to});
        }
    }
}

void solve() {
    dijkstra(s);
    for (int v = 1; v <= n; v++) {
        if (v > 1) cout << ' ';
        // 不可达标记按题面调整，这里统一输出 -1
        if (dis[v] == INF) cout << -1;
        else cout << dis[v];
    }
    cout << '\n';
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    read_input();
    solve();
    return 0;
}
