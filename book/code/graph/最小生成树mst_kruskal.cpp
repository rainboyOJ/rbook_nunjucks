#include <algorithm>
#include <utility>
#include <vector>
using namespace std;

typedef long long ll;

// 一条无向边：从 u 连到 v，边权 w。点编号 1..n，n >= 1。
// 点编号要当数组下标用，写成 int；边权和可能很大，用 ll。
struct Edge {
    int u;  // 起点
    int v;  // 终点
    ll w;   // 边权
};

// 边权小的排在前面，Kruskal 每次都先试当前最小的边。
bool cmp_edge(const Edge &a, const Edge &b) {
    return a.w < b.w;
}

// 找 x 所在集合的根，顺路把路径上的点直接挂到根上（路径压缩）。
int dsu_find(vector<int> &dsu_parent, int x) {
    if (dsu_parent[x] == x) return x;
    dsu_parent[x] = dsu_find(dsu_parent, dsu_parent[x]);
    return dsu_parent[x];
}

// 合并 a、b 所在的两个集合；两端本来就连通时返回 false。
// dsu_size[x] 是 x 当根时该集合的点数，用来按大小合并。
bool dsu_merge(vector<int> &dsu_parent, vector<int> &dsu_size, int a, int b) {
    int root_a = dsu_find(dsu_parent, a);
    int root_b = dsu_find(dsu_parent, b);
    if (root_a == root_b) return false;
    // 小集合接到大集合下面，配合路径压缩，单次操作接近常数。
    if (dsu_size[root_a] < dsu_size[root_b]) swap(root_a, root_b);
    dsu_parent[root_b] = root_a;
    dsu_size[root_a] += dsu_size[root_b];
    return true;
}

// 求最小生成树，按值接收边表，排序不会打乱调用者的边表顺序。
// 返回 true：图连通，answer 存 MST 的总权值；
// 返回 false：图不连通，没有生成树，answer 无意义。
bool kruskal(int n, vector<Edge> edges, ll &answer) {
    sort(edges.begin(), edges.end(), cmp_edge);

    // 并查集要跟着 n 开，放在函数里比固定大小的全局数组安全。
    vector<int> dsu_parent(n + 1);   // dsu_parent[x]：x 的父节点，x 是根时等于 x
    vector<int> dsu_size(n + 1, 1);  // dsu_size[x]：x 是根时集合的大小
    for (int i = 1; i <= n; i++) dsu_parent[i] = i;

    answer = 0;
    int selected = 0;  // 已选边数，一棵生成树正好要 n - 1 条

    for (const Edge &edge : edges) {
        // 自环、两端已经连通的边都会成环，不能选。
        if (!dsu_merge(dsu_parent, dsu_size, edge.u, edge.v)) continue;
        answer += edge.w;
        selected++;
        if (selected == n - 1) break;
    }

    return selected == n - 1;  // 单点图选 0 条边：连通，答案为 0
}
