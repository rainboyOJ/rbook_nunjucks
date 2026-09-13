#include <iostream>
#include <random>
#include <limits>

using namespace std;

// 洛谷 P3369 【模板】普通平衡树
// 基于 FHQ-Treap 的完整 C++ 实现

template<typename T = int, int N = 100005>
struct FHQ {
    int root = 0;
    int tr_idx = 0;

    struct Node {
        int l, r;
        int size;
        unsigned int fix;
        T val;
    } tr[N];

    mt19937 rnd;

    FHQ() {
        rnd.seed(233);
        init();
    }

    void init() {
        root = 0;
        tr_idx = 0;
        tr[0].l = tr[0].r = tr[0].size = 0;
        tr[0].val = 0;
    }

    int new_node(T v) {
        int id = ++tr_idx;
        tr[id].l = tr[id].r = 0;
        tr[id].size = 1;
        tr[id].fix = rnd();
        tr[id].val = v;
        return id;
    }

    void push_up(int u) {
        tr[u].size = tr[tr[u].l].size + tr[tr[u].r].size + 1;
    }

    void split(int u, T v, int &x, int &y) {
        if (!u) {
            x = y = 0;
            return;
        }
        if (tr[u].val <= v) {
            x = u;
            split(tr[u].r, v, tr[u].r, y);
        } else {
            y = u;
            split(tr[u].l, v, x, tr[u].l);
        }
        push_up(u);
    }

    int merge(int x, int y) {
        if (!x || !y) return x + y;
        if (tr[x].fix > tr[y].fix) {
            tr[x].r = merge(tr[x].r, y);
            push_up(x);
            return x;
        } else {
            tr[y].l = merge(x, tr[y].l);
            push_up(y);
            return y;
        }
    }

    void insert(T v) {
        int x, y;
        split(root, v, x, y);
        root = merge(merge(x, new_node(v)), y);
    }

    void del(T v) {
        int x, y, z;
        split(root, v, x, z);
        split(x, v - 1, x, y);
        if (y) {
            y = merge(tr[y].l, tr[y].r);
        }
        root = merge(merge(x, y), z);
    }

    int rank(T v) const {
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

    T kth(int k) const {
        int u = root;
        while (u) {
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
        return 0;
    }

    T pre(T v) const {
        int u = root;
        T ans = numeric_limits<T>::min();
        while (u) {
            if (tr[u].val < v) {
                ans = tr[u].val;
                u = tr[u].r;
            } else {
                u = tr[u].l;
            }
        }
        return ans;
    }

    T succ(T v) const {
        int u = root;
        T ans = numeric_limits<T>::max();
        while (u) {
            if (tr[u].val > v) {
                ans = tr[u].val;
                u = tr[u].l;
            } else {
                u = tr[u].r;
            }
        }
        return ans;
    }
};

FHQ<int, 100005> tree;

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n;
    if (!(cin >> n)) return 0;

    while (n--) {
        int opt, x;
        cin >> opt >> x;
        if (opt == 1) {
            tree.insert(x);
        } else if (opt == 2) {
            tree.del(x);
        } else if (opt == 3) {
            cout << tree.rank(x) << "\n";
        } else if (opt == 4) {
            cout << tree.kth(x) << "\n";
        } else if (opt == 5) {
            cout << tree.pre(x) << "\n";
        } else if (opt == 6) {
            cout << tree.succ(x) << "\n";
        }
    }

    return 0;
}
