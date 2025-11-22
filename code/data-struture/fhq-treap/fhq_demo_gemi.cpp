#include <iostream>
#include <random>
#include <limits> // 用于获取 max/min

template<typename T, int N = 500005>
struct FHQ {
    int root;
    int tr_idx; 

    struct Node {
        int l, r;
        int size;
        unsigned int fix;
        T val;
    } tr[N];

    std::mt19937 rnd;
    
    // 定义极值，用于表示“没找到”
    // 如果是 long long 题目，请确保 T 是 long long
    const T INF_MAX = std::numeric_limits<T>::max();
    const T INF_MIN = std::numeric_limits<T>::min();

    FHQ() { 
        rnd.seed(233); 
        init(); 
    }

    // --- 1. 多组数据必备 (Clear & Init) ---
    
    void init() {
        root = 0;
        tr_idx = 0;
        // 哨兵清理：防止 tr[0] 脏数据污染 size 计算
        tr[0].l = tr[0].r = tr[0].size = 0; 
        tr[0].val = 0;
    }

    void clear() {
        init();
    }

    int size() {
        return tr[root].size;
    }
    
    bool empty() {
        return size() == 0;
    }

    // --- 2. 核心操作 (内部使用) ---

    int new_node(T v) {
        int id = ++tr_idx;
        tr[id].l = tr[id].r = 0;
        tr[id].size = 1;
        tr[id].fix = rnd();
        tr[id].val = v;
        return id;
    }

    void update(int u) {
        tr[u].size = tr[tr[u].l].size + tr[tr[u].r].size + 1;
    }

    void split(int u, T v, int &x, int &y) {
        if (!u) { x = y = 0; return; }
        if (tr[u].val <= v) {
            x = u;
            split(tr[u].r, v, tr[u].r, y);
        } else {
            y = u;
            split(tr[u].l, v, x, tr[u].l);
        }
        update(u);
    }

    int merge(int x, int y) {
        if (!x || !y) return x + y;
        if (tr[x].fix > tr[y].fix) {
            tr[x].r = merge(tr[x].r, y);
            update(x);
            return x;
        } else {
            tr[y].l = merge(x, tr[y].l);
            update(y);
            return y;
        }
    }

    // --- 3. 常用接口 (外部调用) ---

    void insert(T v) {
        int x, y;
        split(root, v, x, y);
        root = merge(merge(x, new_node(v)), y);
    }

    void erase(T v) {
        int x, y, z;
        split(root, v, x, z);
        split(x, v - 1, x, y);
        if (y) y = merge(tr[y].l, tr[y].r);
        root = merge(merge(x, y), z);
    }

    // 查询排名 (比 v 小的数的个数 + 1)
    int rank(T v) {
        int u = root, ans = 0;
        while (u) {
            if (tr[u].val < v) {
                ans += tr[tr[u].l].size + 1;
                u = tr[u].r;
            } else {
                u = tr[u].l;
            }
        }
        return ans + 1;
    }

    // 查询第 k 小
    T kth(int k) {
        int u = root;
        while (true) {
            int l_size = tr[tr[u].l].size;
            if (k <= l_size) {
                u = tr[u].l;
            } else if (k == l_size + 1) {
                return tr[u].val;
            } else {
                k -= l_size + 1;
                u = tr[u].r;
            }
        }
    }

    // --- 4. STL 风格查询接口 ---

    /**
     * lower_bound: 寻找第一个 >= v 的值
     * 返回值: 找到的值，如果没找到返回 INF_MAX
     */
    T lower_bound(T v) {
        int u = root;
        T ans = INF_MAX;
        while (u) {
            if (tr[u].val >= v) {
                ans = tr[u].val; // 记录可行解
                u = tr[u].l;     // 尝试往左找更小的
            } else {
                u = tr[u].r;     // 当前值太小，往右找
            }
        }
        return ans;
    }

    /**
     * upper_bound: 寻找第一个 > v 的值 (等同于 succ)
     * 返回值: 找到的值，如果没找到返回 INF_MAX
     */
    T upper_bound(T v) {
        int u = root;
        T ans = INF_MAX;
        while (u) {
            if (tr[u].val > v) {
                ans = tr[u].val; // 记录可行解
                u = tr[u].l;     // 尝试往左找更小的
            } else {
                u = tr[u].r;     // 当前值 <= v，往右找
            }
        }
        return ans;
    }

    /**
     * pre: 寻找前驱，即 < v 的最大值
     * 返回值: 找到的值，如果没找到返回 INF_MIN
     * 注：STL set 没有直接对应 pre 的函数，通常用 iterator-- 实现
     */
    T pre(T v) {
        int u = root;
        T ans = INF_MIN;
        while (u) {
            if (tr[u].val < v) {
                ans = tr[u].val; // 记录可行解
                u = tr[u].r;     // 尝试往右找更大的
            } else {
                u = tr[u].l;     // 当前值 >= v，往左找
            }
        }
        return ans;
    }
};