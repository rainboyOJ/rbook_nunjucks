/**
 * 
 * 求树上最长的距离:直径
 */
template<int N = maxn>
struct tree_diamter
{

    int dis[N];  // dis[u] 求 u点到以u点为根的最远的叶子的距离
    int next[N]; // next[u] =v 表示
    int st,ed; // 直径的某个起点与终点
    int dfs_diamter(int u, int fa)
    {

        // _maxn 记录 从u开始的最长路径
        // tu 表示 最长点的端点
        dis[u] = 0;
        int tu = u;
        for (int i = e.h[u]; ~i; i = e[i].next)
        {
            int v = e[i].v;
            if (v == fa)
                continue;

            int x = dfs_diamter(v, u); // 从v 开始的最长点
            int len = dis[v] + e[i].w;
            if (len > dis[u])
            {
                dis[u] = len;
                tu = x;
                from[u] = v;
            }
        }
        return tu;
    }

    // 两次dfs求直径
    void two_time_dfs(int start_node = 1) {
        memset(dis,0,sizeof(dis));
        st = dfs_diamter(start_node,0);
        memset(dis,0,sizeof(dis));
        ed = dfs_diamter(st,0);
    }
};