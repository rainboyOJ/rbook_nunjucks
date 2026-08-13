#include <bits/stdc++.h>
using namespace std;

// Treap 旋转平衡树：支持插入 / 删除 / 排名 / kth / 前驱 / 后继
struct Treap {
    using T = int;

    // 线段树节点：child[2] 为左右孩子
    struct Node {
        int child[2] = {0, 0};
        T value = 0;
        int priority = 0;   // 随机优先级（小根堆）
        int count = 0;      // 相同值的个数
        int size = 0;       // 子树节点总数（含 count）
    };

    vector<Node> tree;
    int root = 0;
    mt19937 rng;

    Treap(int max_nodes = 0) : rng(712367821) {
        tree.reserve(max_nodes + 1);
        tree.push_back(Node()); // 0 号节点为空哨兵
    }

    // 新建节点
    int new_node(T value) {
        tree.push_back(Node());
        int id = (int)tree.size() - 1;
        tree[id].value = value;
        tree[id].priority = (int)rng();
        tree[id].count = 1;
        tree[id].size = 1;
        return id;
    }

    // 节点 u 的子树大小，空节点为 0
    int node_size(int u) const {
        return u == 0 ? 0 : tree[u].size;
    }

    // 上推：重算 u 的 size
    void push_up(int u) {
        tree[u].size = node_size(tree[u].child[0]) +
                       node_size(tree[u].child[1]) +
                       tree[u].count;
    }

    // 旋转：direction=0 右旋提升左孩子，direction=1 左旋提升右孩子
    void rotate(int &u, int direction) {
        int v = tree[u].child[direction];
        tree[u].child[direction] = tree[v].child[direction ^ 1];
        tree[v].child[direction ^ 1] = u;
        push_up(u);
        push_up(v);
        u = v;
    }

    void insert(int &u, T value) {
        if (u == 0) {
            u = new_node(value);
            return;
        }
        if (tree[u].value == value) {
            tree[u].count++;
            push_up(u);
            return;
        }

        int direction = value > tree[u].value;
        insert(tree[u].child[direction], value);
        if (tree[tree[u].child[direction]].priority < tree[u].priority) {
            rotate(u, direction);
        }
        push_up(u);
    }

    void insert(T value) {
        insert(root, value);
    }

    void erase(int &u, T value) {
        if (u == 0) return;

        if (tree[u].value == value) {
            if (tree[u].count > 1) {
                tree[u].count--;
                push_up(u);
                return;
            }

            int left = tree[u].child[0];
            int right = tree[u].child[1];
            if (left == 0 || right == 0) {
                u = left + right;
                return;
            }

            int direction = tree[left].priority < tree[right].priority ? 0 : 1;
            rotate(u, direction);
            erase(tree[u].child[direction ^ 1], value);
            push_up(u);
            return;
        }

        int direction = value > tree[u].value;
        erase(tree[u].child[direction], value);
        push_up(u);
    }

    void erase(T value) {
        erase(root, value);
    }

    // 排名（1-based）：最小的值排名 1
    int rank_of(T value) const {
        int u = root;
        int rank = 1;
        while (u != 0) {
            if (value <= tree[u].value) {
                u = tree[u].child[0];
            } else {
                rank += node_size(tree[u].child[0]) + tree[u].count;
                u = tree[u].child[1];
            }
        }
        return rank;
    }

    // 第 k 小
    T kth(int k) const {
        int u = root;
        while (u != 0) {
            int left_size = node_size(tree[u].child[0]);
            if (k <= left_size) {
                u = tree[u].child[0];
            } else if (k <= left_size + tree[u].count) {
                return tree[u].value;
            } else {
                k -= left_size + tree[u].count;
                u = tree[u].child[1];
            }
        }
        return -1;
    }

    // 前驱：小于 value 的最大值
    int predecessor(T value) const {
        int u = root;
        int answer = INT_MIN;
        while (u != 0) {
            if (tree[u].value < value) {
                answer = tree[u].value;
                u = tree[u].child[1];
            } else {
                u = tree[u].child[0];
            }
        }
        return answer;
    }

    // 后继：大于 value 的最小值
    int successor(T value) const {
        int u = root;
        int answer = INT_MAX;
        while (u != 0) {
            if (tree[u].value > value) {
                answer = tree[u].value;
                u = tree[u].child[0];
            } else {
                u = tree[u].child[1];
            }
        }
        return answer;
    }
};

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int m;
    cin >> m;

    Treap treap(m + 5);
    while (m--) {
        int operation, x;
        cin >> operation >> x;

        if (operation == 1) treap.insert(x);
        if (operation == 2) treap.erase(x);
        if (operation == 3) cout << treap.rank_of(x) << '\n';
        if (operation == 4) cout << treap.kth(x) << '\n';
        if (operation == 5) cout << treap.predecessor(x) << '\n';
        if (operation == 6) cout << treap.successor(x) << '\n';
    }

    return 0;
}
