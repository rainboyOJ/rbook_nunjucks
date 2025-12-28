// ---- 求割边 ----
int root; //root点
int dfn[maxn],low[maxn],cnt; //dfn是编号,low是能到达的最小编号
bool is_briage[maxn]; //标记割边
void cut_edge(int u,int from_edge){
    dfn[u] = low[u] = ++cnt; // 编号

    for(int i=head[u];i!=-1;i=E[i].next){ //遍历相邻点
        int v = E[i].v ;//另一个点
        if(! dfn[v] ){ //v点的编号为0，也就是没有被访问
            cut_edge(v,i); //从这个点开始dfs

            //树枝边,用后代的low来更新u的low
            low[u] = std::min(low[u],low[v]);
            
            // 核心: 割边的判断
            if(low[v] > dfn[u] ) is_briage[i] = is_briage[i^1]=1;
        }
        // 判定条件：i 不是进入 u 的那条边的反向边
        else if(i != (from_edge ^ 1)) //v是u的祖先
            low[u] = std::min(low[u],dfn[v]); //回边
    }
}