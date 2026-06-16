
## 找到环上的所有的点


下面的这个代码,是我最喜欢的方式,思维压力低
```cpp
void on_ring[maxe];

bool find_ring = false;
bool vis[maxn];
int parent[maxn];
void find_ring_dfs(int u,int fa) {
  if( find_ring) return 1;
  vis[u] = 1;

  for(int i = e.h[u] ; ~i ; i = e[i].next) {
      int v = e[i].v;
      if( vis[v] ) {
        find_ring = 1;

        int cur = u;
        while( cur != v ) {
          on_ring[cur] = 1;
          cur = parent[cur];
        }
        on_ring[v] = 1;
        return ;
      }

      parent[v] = u;
      find_ring_dfs(v,u);
      if( find_ring) return;
  }
}
```


## 找到换上的所有的边

找到环上的边,标记边在环上

```cpp
void on_ring_edge[maxe];

bool find_ring = false;
bool vis[maxn];


// 记录父亲的,和边的id
struct p { int fa,int eid};
p parent[maxn];

void find_ring_edge_dfs(int u,int fa) {
  if( find_ring) return 1;
  vis[u] = 1;

  for(int i = e.h[u] ; ~i ; i = e[i].next) {
      int v = e[i].v;
      if( vis[v] ) {
        find_ring = 1;
        int cur = u;

        on_ringe_edge[i] = 1;
        while( cur != v ) {
          int eid = parent[cur].eid;
          on_ring_edge[eid] = 1;
          cur = parent[cur].fa;
        }
        return ;
      }

      parent[v] = {u,i};
      find_ring_edge_dfs(v,u);
      if( find_ring) return;
  }
}
```