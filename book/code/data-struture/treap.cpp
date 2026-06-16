#include <bits/stdc++.h>
using namespace std;

struct Treap {
    struct Node {
        int child[2] = {0, 0};
        int value = 0;
        int priority = 0;
        int count = 0;
        int size = 0;
    };

    vector<Node> tree;
    int root = 0;
    mt19937 rng;

    Treap(int max_nodes = 0) : rng(712367821) {
        tree.reserve(max_nodes + 1);
        tree.push_back(Node()); // node 0 is the null sentinel.
    }

    int new_node(int value) {
        tree.push_back(Node());
        int id = (int)tree.size() - 1;
        tree[id].value = value;
        tree[id].priority = (int)rng();
        tree[id].count = 1;
        tree[id].size = 1;
        return id;
    }

    int node_size(int u) const {
        return u == 0 ? 0 : tree[u].size;
    }

    void pushup(int u) {
        tree[u].size = node_size(tree[u].child[0]) +
                       node_size(tree[u].child[1]) +
                       tree[u].count;
    }

    // direction=0: lift left child by right rotation.
    // direction=1: lift right child by left rotation.
    void rotate(int &u, int direction) {
        int v = tree[u].child[direction];
        tree[u].child[direction] = tree[v].child[direction ^ 1];
        tree[v].child[direction ^ 1] = u;
        pushup(u);
        pushup(v);
        u = v;
    }

    void insert(int &u, int value) {
        if (u == 0) {
            u = new_node(value);
            return;
        }
        if (tree[u].value == value) {
            tree[u].count++;
            pushup(u);
            return;
        }

        int direction = value > tree[u].value;
        insert(tree[u].child[direction], value);
        if (tree[tree[u].child[direction]].priority < tree[u].priority) {
            rotate(u, direction);
        }
        pushup(u);
    }

    void insert(int value) {
        insert(root, value);
    }

    void erase(int &u, int value) {
        if (u == 0) return;

        if (tree[u].value == value) {
            if (tree[u].count > 1) {
                tree[u].count--;
                pushup(u);
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
            pushup(u);
            return;
        }

        int direction = value > tree[u].value;
        erase(tree[u].child[direction], value);
        pushup(u);
    }

    void erase(int value) {
        erase(root, value);
    }

    // Rank is 1-based: the smallest value has rank 1.
    int rank_of(int value) const {
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

    int kth(int k) const {
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

    int predecessor(int value) const {
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

    int successor(int value) const {
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
