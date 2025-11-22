// 模板动态化静态

template<typename T,int N=10000>
struct dynamic_static
{
    T * head;
    int idx;
    dynamic_static() : idx(0) { head = new(N * sizeof(T)); }
    ~dynamic_static() : {delete;}

    int get() { return idx++;}


};
