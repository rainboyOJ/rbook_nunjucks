#include <algorithm>
#include <iostream>
#include <vector>
using namespace std;

typedef long long ll;

// 一条无向边：从 u 连到 v，边权 w。点编号 1..n。
// 点编号要当数组下标用，写成 int；边权和可能很大，用 ll。
struct Edge {
    int u;  // 起点
    int v;  // 终点
    ll w;   // 边权
};

vector<Edge> edge_list;  // 边表，读入后按边权升序排序再扫描
vector<int> dsu_parent;  // dsu_parent[x]：x 的父节点，x 是根时等于 x
vector<int> dsu_size;    // dsu_size[x]：x 是根时集合的大小，用来按大小合并
int n;                   // 点数
int m;                   // 边数

// 边权小的排在前面，Kruskal 每次都先试当前最小的边。
bool cmp_edge(const Edge &a, const Edge &b) {
    return a.w < b.w;
}

void dsu_init(int n) {
    dsu_parent.resize(n + 1);
    dsu_size.assign(n + 1, 1);
    for (int i = 1; i <= n; i++) dsu_parent[i] = i;
}

// 找 x 所在集合的根，顺路把路径上的点直接挂到根上（路径压缩）。
int dsu_find(int x) {
    if (dsu_parent[x] == x) return x;
    dsu_parent[x] = dsu_find(dsu_parent[x]);
    return dsu_parent[x];
}

// 合并 a、b 所在的两个集合；两端本来就连通时返回 false。
bool dsu_merge(int a, int b) {
    int root_a = dsu_find(a);
    int root_b = dsu_find(b);
    if (root_a == root_b) return false;
    // 小集合接到大集合下面，配合路径压缩，单次操作接近常数。
    if (dsu_size[root_a] < dsu_size[root_b]) swap(root_a, root_b);
    dsu_parent[root_b] = root_a;
    dsu_size[root_a] += dsu_size[root_b];
    return true;
}

void read_input() {
    cin >> n >> m;
    edge_list.resize(m);
    for (int i = 0; i < m; i++) {
        cin >> edge_list[i].u >> edge_list[i].v >> edge_list[i].w;
    }
}

void solve() {
    sort(edge_list.begin(), edge_list.end(), cmp_edge);
    dsu_init(n);

    ll answer = 0;     // 已选边的边权和
    int selected = 0;  // 已选边数，一棵生成树正好要 n - 1 条

    for (int i = 0; i < m && selected < n - 1; i++) {
        // 自环、两端已经连通的边都会成环，不能选。
        if (!dsu_merge(edge_list[i].u, edge_list[i].v)) continue;
        answer += edge_list[i].w;
        selected++;
    }

    // 选不满 n - 1 条边说明图不连通，没有生成树。
    if (selected != n - 1) cout << "orz" << '\n';
    else cout << answer << '\n';  // 单点图选 0 条边：答案为 0
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    read_input();
    solve();

    return 0;
}
