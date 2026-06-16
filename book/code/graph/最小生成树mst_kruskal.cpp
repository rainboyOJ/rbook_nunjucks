#include <algorithm>
#include <iostream>
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
    vector<int> parent;

    explicit DSU(int n = 0) {
        init(n);
    }

    void init(int n) {
        parent.resize(n + 1);
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
        parent[fa] = fb;
        return true;
    }
};

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n, m;
    cin >> n >> m;

    vector<Edge> edges(m);
    for (auto &edge : edges) {
        cin >> edge.u >> edge.v >> edge.w;
    }

    sort(edges.begin(), edges.end());

    DSU dsu(n);
    long long answer = 0;
    int selected = 0;

    for (const Edge &edge : edges) {
        if (!dsu.merge(edge.u, edge.v)) continue;
        answer += edge.w;
        selected++;
        if (selected == n - 1) break;
    }

    if (selected != n - 1) {
        cout << "orz\n";
    } else {
        cout << answer << '\n';
    }

    return 0;
}
