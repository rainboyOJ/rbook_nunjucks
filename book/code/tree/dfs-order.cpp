int start[maxn],end[maxn];
int dep[maxn],fa[maxn];
int dfs_clock = 0;
int dfs(int d,int u,int pre){
    dep[u] = d;
    fa[u] = pre;
    start[u] = ++dfs_clock;
    int i;
    for(i = head[u]; ~i ; i = e[i].next){
        int v= e[i].v;
        if( v != pre){
            dfs(d+1,v,u);
        }
    }
    end[u] = dfs_clock;
}