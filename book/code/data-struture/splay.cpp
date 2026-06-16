#include <climits>
#include <iostream>
#include <vector>
using namespace std;

struct Splay {
    struct Node {
        int child[2] = {0, 0};
        int parent = 0;
        int value = 0;
        int count = 0;
        int size = 0;
    };

    vector<Node> tree;
    int root = 0;

    Splay(int max_nodes = 0) {
        tree.reserve(max_nodes + 1);
        tree.push_back(Node()); // node 0 is the null sentinel.
    }

    int node_size(int u) const {
        return u == 0 ? 0 : tree[u].size;
    }

    void pushup(int u) {
        if (u == 0) return;
        tree[u].size = node_size(tree[u].child[0]) +
                       node_size(tree[u].child[1]) +
                       tree[u].count;
    }

    int new_node(int value, int parent) {
        tree.push_back(Node());
        int id = (int)tree.size() - 1;
        tree[id].value = value;
        tree[id].count = 1;
        tree[id].size = 1;
        tree[id].parent = parent;
        return id;
    }

    int direction(int u) const {
        int p = tree[u].parent;
        return tree[p].child[1] == u;
    }

    void connect(int child, int parent, int dir) {
        if (parent != 0) tree[parent].child[dir] = child;
        if (child != 0) tree[child].parent = parent;
    }

    void rotate(int x) {
        int y = tree[x].parent;
        int z = tree[y].parent;
        int dx = direction(x);
        int dy = (z == 0 ? 0 : direction(y));
        int middle = tree[x].child[dx ^ 1];

        connect(middle, y, dx);
        connect(y, x, dx ^ 1);
        connect(x, z, dy);

        pushup(y);
        pushup(x);
        if (z == 0) root = x;
    }

    void splay(int x, int goal = 0) {
        if (x == 0) return;
        while (tree[x].parent != goal) {
            int y = tree[x].parent;
            int z = tree[y].parent;
            if (z != goal) {
                if (direction(x) == direction(y)) rotate(y);
                else rotate(x);
            }
            rotate(x);
        }
        if (goal == 0) root = x;
    }

    int find(int value) {
        int u = root;
        int last = 0;
        while (u != 0) {
            last = u;
            if (value == tree[u].value) {
                splay(u);
                return u;
            }
            u = tree[u].child[value > tree[last].value];
        }
        if (last != 0) splay(last);
        return 0;
    }

    void insert(int value) {
        if (root == 0) {
            root = new_node(value, 0);
            return;
        }

        int u = root;
        int parent = 0;
        while (u != 0) {
            parent = u;
            if (value == tree[u].value) {
                tree[u].count++;
                pushup(u);
                splay(u);
                return;
            }
            u = tree[u].child[value > tree[u].value];
        }

        int dir = value > tree[parent].value;
        int id = new_node(value, parent);
        tree[parent].child[dir] = id;
        pushup(parent);
        splay(id);
    }

    void erase(int value) {
        int target = find(value);
        if (target == 0 || tree[target].value != value) return;

        if (tree[target].count > 1) {
            tree[target].count--;
            pushup(target);
            return;
        }

        int left = tree[target].child[0];
        int right = tree[target].child[1];

        if (left == 0) {
            root = right;
            if (root != 0) tree[root].parent = 0;
            return;
        }
        if (right == 0) {
            root = left;
            tree[root].parent = 0;
            return;
        }

        tree[left].parent = 0;
        tree[right].parent = 0;
        root = left;

        int u = left;
        while (tree[u].child[1] != 0) u = tree[u].child[1];
        splay(u);

        tree[root].child[1] = right;
        tree[right].parent = root;
        pushup(root);
    }

    // Rank is 1-based: the smallest value has rank 1.
    int rank_of(int value) {
        int u = root;
        int last = 0;
        int rank = 1;
        while (u != 0) {
            last = u;
            if (value <= tree[u].value) {
                u = tree[u].child[0];
            } else {
                rank += node_size(tree[u].child[0]) + tree[u].count;
                u = tree[u].child[1];
            }
        }
        if (last != 0) splay(last);
        return rank;
    }

    int kth(int k) {
        int u = root;
        while (u != 0) {
            int left_size = node_size(tree[u].child[0]);
            if (k <= left_size) {
                u = tree[u].child[0];
            } else if (k <= left_size + tree[u].count) {
                splay(u);
                return tree[u].value;
            } else {
                k -= left_size + tree[u].count;
                u = tree[u].child[1];
            }
        }
        return -1;
    }

    int predecessor(int value) {
        int u = root;
        int best = 0;
        int answer = INT_MIN;
        while (u != 0) {
            if (tree[u].value < value) {
                best = u;
                answer = tree[u].value;
                u = tree[u].child[1];
            } else {
                u = tree[u].child[0];
            }
        }
        if (best != 0) splay(best);
        return answer;
    }

    int successor(int value) {
        int u = root;
        int best = 0;
        int answer = INT_MAX;
        while (u != 0) {
            if (tree[u].value > value) {
                best = u;
                answer = tree[u].value;
                u = tree[u].child[0];
            } else {
                u = tree[u].child[1];
            }
        }
        if (best != 0) splay(best);
        return answer;
    }
};

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int m;
    cin >> m;

    Splay splay(m + 5);
    while (m--) {
        int operation, x;
        cin >> operation >> x;

        if (operation == 1) splay.insert(x);
        if (operation == 2) splay.erase(x);
        if (operation == 3) cout << splay.rank_of(x) << '\n';
        if (operation == 4) cout << splay.kth(x) << '\n';
        if (operation == 5) cout << splay.predecessor(x) << '\n';
        if (operation == 6) cout << splay.successor(x) << '\n';
    }

    return 0;
}
