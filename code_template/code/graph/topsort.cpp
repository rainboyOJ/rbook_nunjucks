struct TopSort {
    int in_degree[maxn];
    std::queue<int> q; //队列
    std::vector<int> sorted;
    
    void topsort(int n){
        for(int i = 1;i <= n ;++i ) // i: 1->n
        {
            if( in_degree[i] == 0) q.push(i);
        }

        while( !q.empty() )
        {
            int u = q.front();
            sorted.push_back(u);
            q.pop();
            for(int i = e(u) ; ~i;i = e[i].next){
                int v = e[i].v;
                in_degree[v]--;
                if( in_degree[v] == 0) q.push(v);
            }
        }
    }

} topsort;

