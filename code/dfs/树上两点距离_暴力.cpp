
// 求 u-> dim的距离,直径上
// -1 表示没有找到
int dfs_get_two_node_len(int u,int fa,int aim) {
    if( u == aim ) return 0;
    for(int i = e.h[u] ; ~i ;i = e[i].next) {
        int v = e[i].v;
        if ( v == fa) continue;
        int len = dfs_get_two_node_len(v,u,aim);
        if( len  != -1)  return len + e[i].w;
    }
    return -1;
}
