#include <iostream>
#include <algorithm>
#include <optional>
#include <numeric>
#include <vector>
using namespace std;

struct Edge {
    int u;
    int v;
    long long w;

    bool operator<(const Edge &other) const {
        return w < other.w;
    }
};

struct DSU {
    vector<int> parent, size;

    explicit DSU(int n = 0) {
        init(n);
    }

    void init(int n) {
        parent.resize(n + 1);
        size.assign(n + 1, 1);
        iota(parent.begin(), parent.end(), 0);
    }

    int find(int x) {
        if (parent[x] == x) return x;
        return parent[x] = find(parent[x]);
    }

    bool merge(int a, int b) {
        int fa = find(a);
        int fb = find(b);
        if (fa == fb) return false;
        // 小集合接到大集合下面，配合路径压缩。
        if (size[fa] < size[fb]) swap(fa, fb);
        parent[fb] = fa;
        size[fa] += size[fb];
        return true;
    }
};

// 点编号 1..n，n >= 1；边权及权值和须在 long long 范围内。
// 按值接收边表，排序不改变调用者的数据；无生成树时返回 nullopt。
optional<long long> kruskal(int n, vector<Edge> edges) {
    sort(edges.begin(), edges.end());
    DSU dsu(n);
    long long answer = 0;
    int selected = 0;

    for (const Edge &edge : edges) {
        // 自环、两端已连通的边都不能再选。
        if (!dsu.merge(edge.u, edge.v)) continue;
        answer += edge.w;
        selected++;
        if (selected == n - 1) break;
    }

    if (selected != n - 1) return nullopt;
    return answer;  // 单点图选 0 条边，答案为 0。
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n, m;
    cin >> n >> m;
    vector<Edge> edges(m);
    for (auto &edge : edges) cin >> edge.u >> edge.v >> edge.w;

    auto answer = kruskal(n, edges);
    if (!answer.has_value()) cout << "orz\n";
    else cout << *answer << '\n';
    return 0;
}
