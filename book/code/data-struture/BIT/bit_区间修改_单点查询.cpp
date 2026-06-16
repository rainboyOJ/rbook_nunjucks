const int maxn = 1e5+5;
using ll = long long;

template<typename T,int N=maxn>
struct Bit {
    T c[N+5]; // 树状数组, 1-based
    //Bit(){}
    inline int lowbit(int x) { return x & -x;      } // lowbit
    inline int fa(int p)     { return p+lowbit(p); } // update a[p] 时, 下一个要更新的节点
    inline int left(int p)   { return p-lowbit(p); } // query a[1..p] 时, 下一个要求和的节点

    // 单点更新 a[p] += v
    void update(int p, T v){
        for( ; p <= N; p = fa(p) ) c[p] += v;
    }


    // 查询前缀和 a[1..p]
    T query(int p){ //前缀和
        T sum=0;
        for( ;p > 0 ; p = left(p)) sum+= c[p];
        return sum;
    }

    // 区间和
    T query(int l, int r){ 
        return query(r) - query(l-1);
    }

    // 区间加法 a[l..r] += v
    void range_add(int l, int r, T v){
        update(l, v);
        update(r+1, -v);
    }

};
Bit<ll> bit;