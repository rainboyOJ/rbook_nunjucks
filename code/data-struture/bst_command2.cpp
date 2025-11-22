#include <random>

std::mt19937 rnd(233);

// 内存
template<typename T,int size>
struct Mem{
    T mem[size];
    int idx = 0;
    int get() { ++idx;}
};

template <template<typename> class Node,typename T,int maxn=10005>
class bst_node_comm{
public:
    using NodeType = Node<T>;
    Mem<NodeType,maxn> mem;
    int l,r; // 左右子结点
    int fa; // 记录父亲

    int get() {return mem.get();}

    static int rotateLeft(int u) {
        int left  = mem[u].l;
        int lr = mem[left].r
        mem[u].l = 

    }

    static int rotateRight(int u){

    }

    int rank()

};

template<typename T>
struct Node {
    int l,r;
    T val;
    int fix;
    int size;
};


template<typename T,int maxn>
class fhq {

    template<typename T>
    struct fhq_node : public bst_node_comm<fhq_node,T>
    {

    };

};


int main() {
    int a = 1+1;

    return 0;
}