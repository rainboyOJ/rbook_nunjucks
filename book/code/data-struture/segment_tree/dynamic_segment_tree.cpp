#include <bits/stdc++.h>
using namespace std;

// 动态开点线段树 (Dynamic Segment Tree)
// 适用于值域很大（如 1e9），但实际访问节点较少的情况。
// 空间复杂度 O(M log V)，其中 M 为修改次数，V 为值域大小。
struct DynamicSegTree {
    static const int MAX_NODES = 3000005; // 根据 m log V 估算最大节点数
    int ls[MAX_NODES], rs[MAX_NODES]; // 左右儿子指针（数组下标模拟指针）
    long long sum[MAX_NODES]; // 节点维护的信息，如区间和
    int root = 0, tot = 0; // 根节点和节点分配器计数

    // 动态开点：引用传参，当节点为空时分配新节点
    void pushup(int p) {
        sum[p] = sum[ls[p]] + sum[rs[p]];
    }

    // 单点加
    void update(int &p, long long l, long long r, long long x, long long val) {
        if (!p) p = ++tot; // 如果节点不存在，则创建
        if (l == r) {
            sum[p] += val;
            return;
        }
        long long mid = l + ((r - l) >> 1); // 防止溢出
        if (x <= mid) update(ls[p], l, mid, x, val);
        else update(rs[p], mid + 1, r, x, val);
        pushup(p);
    }

    // 区间查询
    long long query(int p, long long l, long long r, long long ql, long long qr) {
        if (!p) return 0; // 节点不存在，说明其表示的区间全是 0
        if (ql <= l && r <= qr) return sum[p];
        long long mid = l + ((r - l) >> 1);
        long long res = 0;
        if (ql <= mid) res += query(ls[p], l, mid, ql, qr);
        if (qr > mid) res += query(rs[p], mid + 1, r, ql, qr);
        return res;
    }
};
