#include <climits>
#include <iostream>
#include <vector>
using namespace std;

// Splay 伸展树：支持插入 / 删除 / 排名 / kth / 前驱 / 后继
struct Splay {
    using T = int;

    // 线段树节点：child[2] 为左右孩子，parent 为父节点
    struct Node {
        int child[2] = {0, 0};
        int parent = 0;
        T value = 0;
        int count = 0;  // 相同值的个数
        int size = 0;   // 子树节点总数（含 count）
    };

    vector<Node> tree;
    int root = 0;

    Splay(int max_nodes = 0) {
        tree.reserve(max_nodes + 1);
        tree.push_back(Node()); // 0 号节点为空哨兵
    }

    // 节点 u 的子树大小，空节点为 0
    int node_size(int u) const {
        return u == 0 ? 0 : tree[u].size;
    }

    // 上推：重算 u 的 size
    void push_up(int u) {
        if (u == 0) return;
        tree[u].size = node_size(tree[u].child[0]) +
                       node_size(tree[u].child[1]) +
                       tree[u].count;
    }

    // 新建节点
    int new_node(T value, int parent) {
        tree.push_back(Node());
        int id = (int)tree.size() - 1;
        tree[id].value = value;
        tree[id].count = 1;
        tree[id].size = 1;
        tree[id].parent = parent;
        return id;
    }

    // u 是父节点的哪个孩子（0 左 1 右）
    int direction(int u) const {
        int p = tree[u].parent;
        return tree[p].child[1] == u;
    }

    // 连接：child 作为 parent 的 dir 方向孩子
    void connect(int child, int parent, int dir) {
        if (parent != 0) tree[parent].child[dir] = child;
        if (child != 0) tree[child].parent = parent;
    }

    // 旋转 x 上移一层
    void rotate(int x) {
        int y = tree[x].parent;
        int z = tree[y].parent;
        int dx = direction(x);
        int dy = (z == 0 ? 0 : direction(y));
        int middle = tree[x].child[dx ^ 1];

        connect(middle, y, dx);
        connect(y, x, dx ^ 1);
        connect(x, z, dy);

        push_up(y);
        push_up(x);
        if (z == 0) root = x;
    }

    // 把 x 伸展到 goal 的孩子（goal=0 表示伸展到根）
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

    // 查找值 value，找到则伸展到根并返回下标，否则返回 0
    int find(T value) {
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

    // 插入值 value
    void insert(T value) {
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
                push_up(u);
                splay(u);
                return;
            }
            u = tree[u].child[value > tree[u].value];
        }

        int dir = value > tree[parent].value;
        int id = new_node(value, parent);
        tree[parent].child[dir] = id;
        push_up(parent);
        splay(id);
    }

    // 删除一个值 value
    void erase(T value) {
        int target = find(value);
        if (target == 0 || tree[target].value != value) return;

        if (tree[target].count > 1) {
            tree[target].count--;
            push_up(target);
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
        push_up(root);
    }

    // 排名（1-based）：最小的值排名 1
    int rank_of(T value) {
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

    // 第 k 小
    T kth(int k) {
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

    // 前驱：小于 value 的最大值
    int predecessor(T value) {
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

    // 后继：大于 value 的最小值
    int successor(T value) {
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
