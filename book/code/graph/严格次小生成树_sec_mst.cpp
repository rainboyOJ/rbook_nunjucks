#include <bits/stdc++.h>
using namespace std;

typedef long long ll;

const int MAXN = 100005;    // 点数上限，按题面 n <= 10^5
const int MAXM = 300005;    // 边数上限，按题面 m <= 3*10^5
const int MAXLG = 18;       // 倍增层数上限，2^17 > 10^5
const int MAXE = 2 * MAXN;  // MST 只有 n-1 条边，按双向存共 2(n-1) 条

const ll INF = (1LL << 62);
const ll NEG = -(1LL << 60);  // “这条路径上没有这种边”的哨兵，边权必须大于它

// 一条无向边：从 u 连到 v，边权 w
struct Edge {
    int u;  // 起点
    int v;  // 终点
    ll w;   // 边权
};

Edge edge_list[MAXM];  // edge_list[i]：读入的第 i 条边（下标从 0 开始）
bool in_mst[MAXM];     // in_mst[i]：第 i 条边是否被 Kruskal 选进 MST

int dsu_parent[MAXN];  // dsu_parent[x]：x 的父节点，x 是根时等于 x
int dsu_size[MAXN];    // dsu_size[x]：x 是根时集合的大小，按大小合并用

int head[MAXN];        // 链式前向星：head[u] 是从 u 出发的第一条 MST 树边
int tree_to[MAXE];     // 这条树边指向的点
int tree_nxt[MAXE];    // 同一出发点的下一条树边
ll tree_weight[MAXE];  // 这条树边的边权
int tree_edge_cnt;     // 已加入的树边条数

int depth[MAXN];          // depth[u]：u 在 MST 上的深度，根为 1
int up[MAXLG][MAXN];      // up[k][u]：u 向上跳 2^k 步到达的点
ll mx1[MAXLG][MAXN];      // mx1[k][u]：这段路径上的最大边权
ll mx2[MAXLG][MAXN];      // mx2[k][u]：这段路径上严格小于最大值的次大边权

int n;  // 点数
int m;  // 边数

// 边权小的排在前面，Kruskal 每次都先试当前最小的边。
bool cmp_edge(const Edge &a, const Edge &b) {
    return a.w < b.w;
}

void dsu_init(int n) {
    for (int i = 1; i <= n; i++) {
        dsu_parent[i] = i;
        dsu_size[i] = 1;
    }
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

// 在 MST 上连一条无向边 u--v，边权为 w。
void add_tree_edge(int u, int v, ll w) {
    tree_edge_cnt++;
    tree_to[tree_edge_cnt] = v;
    tree_weight[tree_edge_cnt] = w;
    tree_nxt[tree_edge_cnt] = head[u];
    head[u] = tree_edge_cnt;
}

// 用 Kruskal 求最小生成树，同时把选中的边建成树。
// 返回 MST 的总权值；图不连通时返回 -1。
ll build_mst() {
    sort(edge_list, edge_list + m, cmp_edge);
    dsu_init(n);

    ll total = 0;      // MST 的总权值
    int selected = 0;  // 已选边数，一棵生成树正好要 n - 1 条

    for (int i = 0; i < m && selected < n - 1; i++) {
        // 自环、两端已经连通的边都会成环，不能选。
        if (!dsu_merge(edge_list[i].u, edge_list[i].v)) continue;
        in_mst[i] = true;
        total += edge_list[i].w;
        selected++;
        add_tree_edge(edge_list[i].u, edge_list[i].v, edge_list[i].w);
        add_tree_edge(edge_list[i].v, edge_list[i].u, edge_list[i].w);
    }

    if (selected != n - 1) return -1;
    return total;
}

// 在 MST 上递归，预处理每个点的深度、父亲和到父亲的边权。
void dfs_tree(int u, int parent_node) {
    for (int i = head[u]; i != 0; i = tree_nxt[i]) {
        int v = tree_to[i];
        if (v == parent_node) continue;
        depth[v] = depth[u] + 1;
        up[0][v] = u;
        mx1[0][v] = tree_weight[i];
        mx2[0][v] = NEG;  // 只有一条边，没有次大边
        dfs_tree(v, u);
    }
}

// 把候选值 x 合并进“最大边 largest / 严格次大边 second_largest”这一对里。
// 只有严格小于当前最大值才可能当次大边，所以最大的边永远进不了第二位。
void add_candidate(ll x, ll &largest, ll &second_largest) {
    if (x == NEG) return;
    if (x > largest) {
        second_largest = largest;
        largest = x;
    } else if (x < largest && x > second_largest) {
        second_largest = x;
    }
}

// 把「u 向上跳 2^k 步」这段路径上的最大、次大边权合并进 (largest, second_largest)。
void merge_segment(int k, int u, ll &largest, ll &second_largest) {
    add_candidate(mx1[k][u], largest, second_largest);
    add_candidate(mx2[k][u], largest, second_largest);
}

// 倍增预处理：up、mx1、mx2 三张表。
void build_lca() {
    // 全局数组默认是 0，而 0 也是合法边权，所以先全部设成“不存在”哨兵。
    for (int k = 0; k < MAXLG; k++) {
        for (int u = 0; u <= n; u++) {
            mx1[k][u] = NEG;
            mx2[k][u] = NEG;
        }
    }

    depth[1] = 1;
    up[0][1] = 0;
    dfs_tree(1, 0);

    for (int k = 1; k < MAXLG; k++) {
        for (int u = 1; u <= n; u++) {
            int mid = up[k - 1][u];  // 先跳 2^(k-1) 步，到达中点
            up[k][u] = up[k - 1][mid];
            mx1[k][u] = NEG;
            mx2[k][u] = NEG;
            // 前半段和后半段各贡献两个候选值，合并成这一段的最大、次大边权。
            merge_segment(k - 1, u, mx1[k][u], mx2[k][u]);
            merge_segment(k - 1, mid, mx1[k][u], mx2[k][u]);
        }
    }
}

// 查询 MST 上 a 到 b 的路径：最大边权 largest 与严格次大边权 second_largest。
void path_max_two(int a, int b, ll &largest, ll &second_largest) {
    largest = NEG;
    second_largest = NEG;

    // 先把深的点跳到和另一个点同深度。
    if (depth[a] < depth[b]) swap(a, b);
    int diff = depth[a] - depth[b];
    for (int k = 0; k < MAXLG; k++) {
        if (diff >> k & 1) {
            merge_segment(k, a, largest, second_largest);
            a = up[k][a];
        }
    }

    if (a == b) return;  // 其中一个点就是祖先，路径已经走完

    // 两个点一起往上跳，直到它们的父亲相同。
    for (int k = MAXLG - 1; k >= 0; k--) {
        if (up[k][a] != up[k][b]) {
            merge_segment(k, a, largest, second_largest);
            merge_segment(k, b, largest, second_largest);
            a = up[k][a];
            b = up[k][b];
        }
    }

    // 最后各补一条到 LCA 的边。
    merge_segment(0, a, largest, second_largest);
    merge_segment(0, b, largest, second_largest);
}

void read_input() {
    cin >> n >> m;
    for (int i = 0; i < m; i++) {
        cin >> edge_list[i].u >> edge_list[i].v >> edge_list[i].w;
    }
}

void solve() {
    ll mst_total = build_mst();
    if (mst_total < 0) {  // 图不连通，不存在生成树
        cout << -1 << '\n';
        return;
    }

    build_lca();

    ll answer = INF;
    for (int i = 0; i < m; i++) {
        if (in_mst[i]) continue;  // 只枚举非树边

        // 加入非树边会成环，只能删环上的一条边才能变回生成树。
        // 删严格小于它的最大边，才能让总权值严格变大且增量最小。
        ll largest, second_largest;
        path_max_two(edge_list[i].u, edge_list[i].v, largest, second_largest);

        ll removed = NEG;
        if (largest < edge_list[i].w) removed = largest;
        else if (second_largest < edge_list[i].w) removed = second_largest;

        if (removed != NEG) {
            answer = min(answer, mst_total + edge_list[i].w - removed);
        }
    }

    if (answer == INF) cout << -1 << '\n';  // 所有非树边都换不出严格更大的生成树
    else cout << answer << '\n';
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    read_input();
    solve();

    return 0;
}
