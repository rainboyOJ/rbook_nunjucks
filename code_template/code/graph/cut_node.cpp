// ---- 求割点 ----
int root; //root点
int dfn[maxn],low[maxn],cnt; //dfn是编号,low是能到达的最小编号
bool cut[maxn]; //标记割点
void cut_node(int u,int fa){
    dfn[u] = low[u] = ++cnt; // 编号

    int child=0;//记录root的孩子数
    
    int i;
    for(i=head[u];i!=-1;i=E[i].next){ //遍历相邻点
        int v = E[i].v ;//另一个点
        if(! dfn[v] ){ //v点的编号为0，也就是没有被访问
            child++; // u的孩子数加1
            cut_node(v,fa); //从这个点开始dfs

            //树枝边,用后代的low来更新u的low
            low[u] = min(low[u],low[v]);
            
            if(low[v] >= dfn[u] && u != root) // 情况2
                cut[u] =1;
        }
        // 处理返祖边
        //注意:v可能是u的父亲,但没有关系,最多low[u] == dfn[fa[u]]
        // dfn[v] < dfn[u] 说明v是u的祖先, 
        // 在无向图上其实不可能 dfn[v] > dfn[u]
        // 因为: v 是一个已经访问过的点, 如果dfn[v] > dfn[u] 说明u是v的祖先, 那么v在u的子树上, 
        // 根据dfs 的性质, 应该先访问u, 再访问v,但此时v已经被访问, 所以不可能出现dfn[v] > dfn[u]的情况
        else if(dfn[v] < dfn[u] && v != fa) //v是u的祖先
            low[u] = std::min(low[u],dfn[v]); //回边
    }
    // 退出这个点
    if( u == root && child >1)  //情况1:
        cut[u] = 1;
}