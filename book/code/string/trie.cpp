#include <bits/stdc++.h>
using namespace std;

struct Trie {
    struct Node {
        array<int, 26> next{};
        int pass = 0;
        int end = 0;
    };

    vector<Node> tree;

    Trie() {
        tree.push_back(Node());
    }

    void insert(const string &s) {
        int u = 0;
        tree[u].pass++;
        for (char ch : s) {
            int c = ch - 'a';
            if (tree[u].next[c] == 0) {
                tree[u].next[c] = (int)tree.size();
                tree.push_back(Node());
            }
            u = tree[u].next[c];
            tree[u].pass++;
        }
        tree[u].end++;
    }

    bool contains(const string &s) const {
        int u = 0;
        for (char ch : s) {
            int c = ch - 'a';
            if (tree[u].next[c] == 0) return false;
            u = tree[u].next[c];
        }
        return tree[u].end > 0;
    }

    int count_prefix(const string &prefix) const {
        int u = 0;
        for (char ch : prefix) {
            int c = ch - 'a';
            if (tree[u].next[c] == 0) return 0;
            u = tree[u].next[c];
        }
        return tree[u].pass;
    }
};

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n, q;
    cin >> n >> q;

    Trie trie;
    for (int i = 0; i < n; i++) {
        string s;
        cin >> s;
        trie.insert(s);
    }

    while (q--) {
        int op;
        string s;
        cin >> op >> s;
        if (op == 1) {
            cout << (trie.contains(s) ? "Yes" : "No") << '\n';
        } else {
            cout << trie.count_prefix(s) << '\n';
        }
    }

    return 0;
}
