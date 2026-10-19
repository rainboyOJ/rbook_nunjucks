#include <algorithm>
#include <queue>
#include <utility>
#include <vector>

// 差分约束：把 x_v - x_u <= w 统一成边 u -> v 权 w，跑单源最短路判可行性、求差值上界。
// 点编号 1..n。add_leq / add_geq / add_eq 只负责统一不等号方向，不检查输入。
// 边权可正可负，所以用 SPFA；出现负环表示约束矛盾。

struct DiffConstraints {
    using ll = long long;
    static constexpr ll INF = (1LL << 62);

    struct Edge {
        int to;
        ll w;
    };

    int n;
    std::vector<std::vector<Edge>> g;
    std::vector<ll> dis;       // feasible() 后是一组可行解；shortest(s) 后是 s 到各点的最短路
    std::vector<int> cnt;      // 当前 dis 对应的路径边数，达到 n 说明绕出了环
    std::vector<char> in_queue;

    explicit DiffConstraints(int n)
        : n(n), g(n + 1), dis(n + 1, 0), cnt(n + 1, 0), in_queue(n + 1, false) {}

    // 约束 x_v - x_u <= w，也就是 x_v <= x_u + w。
    void add_leq(int u, int v, ll w) {
        g[u].push_back({v, w});
    }

    // 约束 x_u - x_v >= w，也就是 x_v <= x_u - w。
    void add_geq(int u, int v, ll w) {
        g[u].push_back({v, -w});
    }

    // 约束 x_u == x_v，拆成 x_u - x_v <= 0 与 x_v - x_u <= 0。
    void add_eq(int u, int v) {
        add_leq(u, v, 0);
        add_leq(v, u, 0);
    }

    // 全局可行性：所有点距离取 0 并全部入队，等价于加一个到所有点都是 0 权边的超级源点。
    // 返回 false 表示存在负环、约束无解；返回 true 时 dis 就是一组可行解。
    bool feasible() {
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
                if (dis[u] + e.w >= dis[e.to]) continue; // 没有严格改善
                dis[e.to] = dis[u] + e.w;
                cnt[e.to] = cnt[u] + 1;
                if (cnt[e.to] >= n) return false;        // 更短的路径用了至少 n 条边，必然有负环
                if (!in_queue[e.to]) {
                    q.push(e.to);
                    in_queue[e.to] = true;
                }
            }
        }
        return true;
    }

    // 单源最短路：dis[source] = 0，其余为 INF。返回 false 表示从 source 可达负环。
    // 与 feasible() 分开是因为很多题只要求某个点在它自己的连通块里无矛盾。
    bool shortest(int source) {
        std::fill(dis.begin(), dis.end(), INF);
        std::fill(cnt.begin(), cnt.end(), 0);
        std::fill(in_queue.begin(), in_queue.end(), false);
        dis[source] = 0;
        std::queue<int> q;
        q.push(source);
        in_queue[source] = true;

        while (!q.empty()) {
            int u = q.front();
            q.pop();
            in_queue[u] = false;

            for (const auto& e : g[u]) {
                if (e.w >= INF - dis[u]) continue;       // 先判断再相加，避免溢出
                ll candidate = dis[u] + e.w;
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
};
