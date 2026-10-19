// 差分约束求 x_s - x_t 的最大值。
// 输入：第一行 n m s t；接下来 m 行 a b c，表示约束 x_a - x_b <= c（点编号 1..n）。
// 输出：x_s - x_t 的最大值；约束矛盾输出 NO；差值没有上界输出 UNBOUNDED。

#include <algorithm>
#include <iostream>
#include <queue>
#include <vector>

const long long INF = (1LL << 62);

struct Edge {
    int to;
    long long w;
};

std::vector<std::vector<Edge>> g;
std::vector<long long> dis;
std::vector<int> cnt;
std::vector<char> in_queue;

void add_leq(int u, int v, long long w) {
    g[u].push_back({v, w});
}

// 全局可行性：所有点距离取 0 并全部入队，等价于加一个 0 权超级源点。
bool feasible(int n) {
    std::fill(dis.begin(), dis.end(), 0);
    std::fill(cnt.begin(), cnt.end(), 0);
    std::fill(in_queue.begin(), in_queue.end(), true);
    std::queue<int> q;
    for (int u = 1; u <= n; ++u) q.push(u);

    while (!q.empty()) {
        int u = q.front();
        q.pop();
        in_queue[u] = false;
        for (const auto& e : g[u]) {
            if (dis[u] + e.w >= dis[e.to]) continue;
            dis[e.to] = dis[u] + e.w;
            cnt[e.to] = cnt[u] + 1;
            if (cnt[e.to] >= n) return false;
            if (!in_queue[e.to]) {
                q.push(e.to);
                in_queue[e.to] = true;
            }
        }
    }
    return true;
}

// 单源最短路；返回 false 表示从 source 可达负环。
bool shortest(int n, int source) {
    std::fill(dis.begin(), dis.end(), INF);
    std::fill(cnt.begin(), cnt.end(), 0);
    std::fill(in_queue.begin(), in_queue.end(), false);
    dis[source] = 0;
    in_queue[source] = true;
    std::queue<int> q;
    q.push(source);

    while (!q.empty()) {
        int u = q.front();
        q.pop();
        in_queue[u] = false;
        for (const auto& e : g[u]) {
            if (e.w >= INF - dis[u]) continue;
            long long candidate = dis[u] + e.w;
            if (candidate >= dis[e.to]) continue;
            dis[e.to] = candidate;
            cnt[e.to] = cnt[u] + 1;
            if (cnt[e.to] >= n) return false;
            if (!in_queue[e.to]) {
                q.push(e.to);
                in_queue[e.to] = true;
            }
        }
    }
    return true;
}

int main() {
    std::ios::sync_with_stdio(false);
    std::cin.tie(nullptr);

    int n, m, s, t;
    if (!(std::cin >> n >> m >> s >> t)) return 0;

    g.assign(n + 1, {});
    dis.assign(n + 1, 0);
    cnt.assign(n + 1, 0);
    in_queue.assign(n + 1, false);

    for (int i = 0; i < m; ++i) {
        int a, b;
        long long c;
        std::cin >> a >> b >> c;
        add_leq(b, a, c); // x_a - x_b <= c 就是 x_a <= x_b + c
    }

    if (!feasible(n)) {
        std::cout << "NO\n"; // 约束自相矛盾
        return 0;
    }

    shortest(n, t); // 已确认无负环，t 到 s 的最短路就是 x_s - x_t 的上界
    if (dis[s] == INF) {
        std::cout << "UNBOUNDED\n"; // t 到不了 s，差值可以被拉得任意大
    } else {
        std::cout << dis[s] << '\n';
    }
    return 0;
}
