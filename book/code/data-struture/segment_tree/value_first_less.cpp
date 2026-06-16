#include <bits/stdc++.h>
using namespace std;

struct MinSegmentTree {
    static const int INF = 1e9;

    int n = 0;
    vector<int> tree;

    MinSegmentTree(int n = 0) {
        init(n);
    }

    void init(int size) {
        n = size;
        tree.assign(n * 4 + 5, INF);
    }

    void pull(int p) {
        tree[p] = min(tree[p << 1], tree[p << 1 | 1]);
    }

    void update_min(int pos, int value, int l, int r, int p = 1) {
        if (l == r) {
            tree[p] = min(tree[p], value);
            return;
        }

        int mid = (l + r) >> 1;
        if (pos <= mid) update_min(pos, value, l, mid, p << 1);
        else update_min(pos, value, mid + 1, r, p << 1 | 1);
        pull(p);
    }

    int query_min(int ql, int qr, int l, int r, int p = 1) const {
        if (ql > qr) return INF;
        if (ql <= l && r <= qr) return tree[p];

        int mid = (l + r) >> 1;
        int answer = INF;
        if (ql <= mid) answer = min(answer, query_min(ql, qr, l, mid, p << 1));
        if (qr > mid) answer = min(answer, query_min(ql, qr, mid + 1, r, p << 1 | 1));
        return answer;
    }
};

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n, q;
    cin >> n >> q;

    vector<int> a(n + 1);
    vector<int> values;
    values.reserve(n);

    for (int i = 1; i <= n; i++) {
        cin >> a[i];
        values.push_back(a[i]);
    }

    sort(values.begin(), values.end());
    values.erase(unique(values.begin(), values.end()), values.end());

    MinSegmentTree seg((int)values.size());

    for (int i = 1; i <= n; i++) {
        int rank = lower_bound(values.begin(), values.end(), a[i]) - values.begin() + 1;
        seg.update_min(rank, i, 1, seg.n);
    }

    while (q--) {
        int x;
        cin >> x;

        // 所有小于 x 的值，正好落在压缩值域的 [1, last_less]。
        int last_less = lower_bound(values.begin(), values.end(), x) - values.begin();
        int pos = seg.query_min(1, last_less, 1, seg.n);

        if (pos == MinSegmentTree::INF) cout << n + 1 << '\n';
        else cout << pos << '\n';
    }

    return 0;
}
