#include <iostream>
using namespace std;

struct DynamicSegTree {
    static const int MAX_NODES = 3000005; 
    int ls[MAX_NODES], rs[MAX_NODES]; 
    long long sum[MAX_NODES]; 
    int root = 0, tot = 0; 

    void pushup(int p) {
        sum[p] = sum[ls[p]] + sum[rs[p]];
    }

    void update(int &p, long long l, long long r, long long x, long long val) {
        if (!p) p = ++tot;
        if (l == r) {
            sum[p] += val;
            return;
        }
        long long mid = l + ((r - l) >> 1);
        if (x <= mid) update(ls[p], l, mid, x, val);
        else update(rs[p], mid + 1, r, x, val);
        pushup(p);
    }

    long long query(int p, long long l, long long r, long long ql, long long qr) {
        if (!p) return 0;
        if (ql <= l && r <= qr) return sum[p];
        long long mid = l + ((r - l) >> 1);
        long long res = 0;
        if (ql <= mid) res += query(ls[p], l, mid, ql, qr);
        if (qr > mid) res += query(rs[p], mid + 1, r, ql, qr);
        return res;
    }
};

int main() {
    // 例题：维护值域 [1, 10^9] 的单点修改、区间求和
    DynamicSegTree tree;
    long long limit = 1e9;
    
    // 在位置 1000 加上 5
    tree.update(tree.root, 1, limit, 1000, 5);
    // 在位置 2000000 加上 10
    tree.update(tree.root, 1, limit, 2000000, 10);
    // 在位置 999999999 加上 20
    tree.update(tree.root, 1, limit, 999999999, 20);

    // 查询区间 [1, 10000] 的和，应该只包含位置 1000 的 5
    cout << tree.query(tree.root, 1, limit, 1, 10000) << "\n"; // 输出: 5

    // 查询区间 [1, 10^9] 的和，包含所有修改，应为 35
    cout << tree.query(tree.root, 1, limit, 1, limit) << "\n"; // 输出: 35
    
    // 输出共开辟的节点数，以说明空间的高效利用
    cout << "Allocated nodes: " << tree.tot << "\n";

    return 0;
}
