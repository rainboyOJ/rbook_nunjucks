#include <bits/stdc++.h>
using namespace std;

// 有向图欧拉路 / 欧拉回路竞赛精简版（Hierholzer，不用 struct）
// 输入 n m 与 m 条有向边 u v（u -> v），输出顶点序列，无解输出 NO
// 判定：恰一个起点出度 = 入度 + 1，恰一个终点入度 = 出度 + 1，其余出入度相等（回路则全部相等）
// 弱连通不用单独判：走不完所有边（path 长度 != m+1）就是不弱连通
// 口诀：走不动了再记点，记完 reverse

int n, m;
vector<vector<pair<int, int>>> g; // 邻接表 {终点, 边编号}
vector<int> in, out, used, cur, path;

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
            auto e = g[u][cur[u]++]; // 用掉这条出边，走向下一点
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
    in.assign(n + 1, 0);
    out.assign(n + 1, 0);
    used.assign(m + 1, 0);
    cur.assign(n + 1, 0);
    for (int id = 1; id <= m; id++) {
        int u, v;
        cin >> u >> v;
        g[u].push_back({v, id});
        out[u]++;
        in[v]++;
    }

    int s = -1, sc = 0, tc = 0;
    for (int i = 1; i <= n; i++) {
        if (out[i] == in[i] + 1) sc++, s = i; // 起点候选
        else if (in[i] == out[i] + 1) tc++;   // 终点候选
        else if (in[i] != out[i]) {
            cout << "NO\n";
            return 0;
        }
    }
    // 易错点：必须恰 0 个或恰 1 对起终点；多对时 path 长度能骗过检查但输出非法
    if (sc > 1 || tc > 1) {
        cout << "NO\n";
        return 0;
    }
    if (s == -1) { // 全平衡是回路，任取一个有出边的点出发
        for (int i = 1; i <= n; i++)
            if (out[i]) { s = i; break; }
    }
    if (s == -1) { // 边界：没有任何边，欧拉路是空序列
        cout << '\n';
        return 0;
    }

    hierholzer(s);
    if ((int)path.size() != m + 1) { // 边没走完，说明底图不弱连通
        cout << "NO\n";
        return 0;
    }
    reverse(path.begin(), path.end());
    for (int i = 0; i < (int)path.size(); i++) cout << path[i] << " \n"[i + 1 == (int)path.size()];
    return 0;
}
