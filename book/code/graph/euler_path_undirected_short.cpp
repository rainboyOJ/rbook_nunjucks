#include <bits/stdc++.h>
using namespace std;

// 无向图欧拉路 / 欧拉回路竞赛精简版（Hierholzer，不用 struct）
// 输入 n m 与 m 条边，输出一条欧拉路的顶点序列，无解输出 NO
// 判定：奇度点只能有 0 个（回路）或 2 个（路，从奇点出发）
// 连通性不用单独判：走不完所有边（path 长度 != m+1）就是不连通
// 口诀：走不动了再记点，记完 reverse

int n, m;
vector<vector<pair<int, int>>> g; // 邻接表 {终点, 边编号}
vector<int> deg, used, cur, path; // used: 边已走；cur: 当前弧

void hierholzer(int s) {
    vector<int> st = {s};
    while (!st.empty()) {
        int u = st.back();
        // 跳过已走的边；cur 保证每条边只被扫过常数次，总复杂度 O(n+m)
        while (cur[u] < (int)g[u].size() && used[g[u][cur[u]].second]) cur[u]++;
        if (cur[u] == (int)g[u].size()) {
            path.push_back(u); // 走不动才记点（后序），reverse 后才是答案
            st.pop_back();
        } else {
            auto e = g[u][cur[u]++]; // 用掉这条边，走向下一点
            used[e.second] = 1;
            st.push_back(e.first);
        }
    }
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cin >> n >> m;
    g.assign(n + 1, {});
    deg.assign(n + 1, 0);
    used.assign(m + 1, 0);
    cur.assign(n + 1, 0);
    for (int id = 1; id <= m; id++) {
        int u, v;
        cin >> u >> v;
        g[u].push_back({v, id});
        g[v].push_back({u, id}); // 两个方向共用编号，不会来回走同一条边
        deg[u] += (u == v ? 2 : 1); // 易错点：自环度数 +2
        if (u != v) deg[v]++;
    }

    int s = -1, odd = 0;
    for (int i = 1; i <= n; i++) {
        if (deg[i] % 2) odd++, s = i; // 奇点只可能是端点
    }
    if (odd != 0 && odd != 2) {
        cout << "NO\n";
        return 0;
    }
    if (s == -1) { // 全偶度是回路，任取一个有边的点出发
        for (int i = 1; i <= n; i++)
            if (deg[i]) { s = i; break; }
    }
    if (s == -1) { // 边界：没有任何边，欧拉路是空序列
        cout << '\n';
        return 0;
    }

    hierholzer(s);
    if ((int)path.size() != m + 1) { // 边没走完，说明有边的点不连通
        cout << "NO\n";
        return 0;
    }
    reverse(path.begin(), path.end());
    for (int i = 0; i < (int)path.size(); i++) cout << path[i] << " \n"[i + 1 == (int)path.size()];
    return 0;
}
