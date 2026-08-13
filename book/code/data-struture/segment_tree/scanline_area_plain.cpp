#include <bits/stdc++.h>
using namespace std;

using i64 = long long;

// 扫描线事件：一条竖直边，delta=1 为入边，-1 为出边
struct Event {
    int x1, x2, y, delta;
    bool operator<(const Event &other) const {
        return y < other.y;
    }
};

// 扫描线线段树：维护区间被覆盖的次数与覆盖长度
struct ScanlineSegmentTree {
    struct Node {
        int cover = 0;      // 区间被完整覆盖的次数（不做下传，只统计根）
        int covered_len = 0; // 区间内被覆盖的长度
    };

    int n = 0;              // 坐标范围
    vector<Node> tree;      // 线段树数组

    explicit ScanlineSegmentTree(int n = 0) {
        init(n);
    }

    void init(int size) {
        n = size;
        tree.assign(n * 4 + 5, Node{});
    }

    // 左儿子 / 右儿子的节点编号
    static int lson(int p) { return p << 1; }
    static int rson(int p) { return p << 1 | 1; }

    // 区间 [l, r] 的中点
    static int mid(int l, int r) { return (l + r) >> 1; }

    // 上推：根据完整覆盖次数与孩子信息更新覆盖长度
    void push_up(int p, int l, int r) {
        if (tree[p].cover > 0) {
            tree[p].covered_len = r - l + 1;
        } else if (l == r) {
            tree[p].covered_len = 0;
        } else {
            tree[p].covered_len = tree[lson(p)].covered_len + tree[rson(p)].covered_len;
        }
    }

    // 区间加覆盖次数：给 [ql, qr] 增加 v（v = 1 或 -1）
    void add(int ql, int qr, int v, int l, int r, int p = 1) {
        if (ql <= l && r <= qr) {
            tree[p].cover += v;
            push_up(p, l, r);
            return;
        }
        int m = mid(l, r);
        if (ql <= m) add(ql, qr, v, l, m, lson(p));
        if (m < qr) add(ql, qr, v, m + 1, r, rson(p));
        push_up(p, l, r);
    }

    // 整棵树的覆盖长度
    int query_all() const {
        return tree[1].covered_len;
    }
};

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n;
    cin >> n;

    vector<Event> events;
    int max_x = 0;
    for (int i = 0; i < n; ++i) {
        int x1, y1, x2, y2;
        cin >> x1 >> y1 >> x2 >> y2;
        events.push_back({x1, x2 - 1, y1, 1});
        events.push_back({x1, x2 - 1, y2, -1});
        max_x = max(max_x, x2 - 1);
    }

    sort(events.begin(), events.end());
    ScanlineSegmentTree seg(max_x + 2);

    i64 ans = 0;
    int last_y = events.empty() ? 0 : events[0].y;
    int last_len = 0;

    for (const auto &e : events) {
        ans += 1LL * last_len * (e.y - last_y);
        if (e.x1 <= e.x2) seg.add(e.x1, e.x2, e.delta, 1, max_x);
        last_len = seg.query_all();
        last_y = e.y;
    }

    cout << ans << '\n';
    return 0;
}
