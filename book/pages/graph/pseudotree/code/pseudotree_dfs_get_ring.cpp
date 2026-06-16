// loop: 存储环路上的节点,loop_cnt: 环路上的节点数量
int loop[maxn],loop_cnt;
// dfn: 节点在DFS过程中的访问时间戳 (discovery time)
// dfn_idx: 时间戳计数器
int dfn[maxn],dfn_idx;
int fa[maxn]; // fa: 在DFS树中，节点的父节点

/**
 * @brief 通过DFS寻找图中的环.
 * 该函数适用于有向图，它寻找由前向边构成的环.
 * @param u 当前访问的节点
 * @param father u在DFS树中的父节点
 */
void get_ring(int u, int father)
{
    // 为当前节点u分配一个访问时间戳
    dfn[u] = ++dfn_idx;
    // 记录u的父节点
    fa[u] = father;

    for (int i = e.h[u]; i != -1; i = e[i].next)
    {
        int v = e[i].v; // 邻接点
        
        // 如果v已经被访问过
        if (dfn[v])
        {
            // 如果v是u的祖先节点(返祖边),则跳过
            if (dfn[v] < dfn[u]) continue; // 返祖边略过

            // 找到一个环: u -> v, 并且v是u的后代(前向边)
            // v的时间戳比u大，说明v在u的DFS子树中
            
            // 从v开始，通过父节点指针回溯到u，记录路径上的所有节点
            for( ; v != u ; v = fa[v]) loop[++loop_cnt] = v;
            loop[++loop_cnt] = u;
        }
        else get_ring(v, u);
    }
}