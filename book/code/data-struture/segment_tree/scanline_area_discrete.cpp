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

// 扫描线线段树（离散化版）：维护区间被覆盖的次数与覆盖长度
struct ScanlineSegmentTree {
    struct Node {
        int cover = 0;       // 区间被完整覆盖的次数（不做下传，只统计根）
        i64 covered_len = 0; // 区间内被覆盖的长度（按离散化坐标计算）
    };

    int n = 0;                  // 离散化后的段数
    vector<Node> tree;          // 线段树数组
    const vector<int> *xs;      // 离散化坐标，xs[i] 为第 i 段的左端点

    ScanlineSegmentTree(int n = 0, const vector<int> &coord = {})
        : xs(&coord) {
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
            tree[p].covered_len = 1LL * (*xs)[r] - (*xs)[l - 1];
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
    i64 query_all() const {
        return tree[1].covered_len;
    }
};

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n;
    cin >> n;

    vector<Event> events;
    vector<int> xs;
    for (int i = 0; i < n; ++i) {
        int x1, y1, x2, y2;
        cin >> x1 >> y1 >> x2 >> y2;
        events.push_back({x1, x2, y1, 1});
        events.push_back({x1, x2, y2, -1});
        xs.push_back(x1);
        xs.push_back(x2);
    }

    sort(events.begin(), events.end());
    sort(xs.begin(), xs.end());
    xs.erase(unique(xs.begin(), xs.end()), xs.end());

    int segment_count = max(0, (int)xs.size() - 1);
    if (segment_count == 0) {
        cout << 0 << '\n';
        return 0;
    }

    ScanlineSegmentTree seg(segment_count, xs);
    i64 ans = 0;
    int last_y = events.empty() ? 0 : events[0].y;
    i64 last_len = 0;

    for (const auto &e : events) {
        ans += last_len * (e.y - last_y);

        int l = lower_bound(xs.begin(), xs.end(), e.x1) - xs.begin() + 1;
        int r = lower_bound(xs.begin(), xs.end(), e.x2) - xs.begin();
        if (l <= r) seg.add(l, r, e.delta, 1, segment_count);

        last_len = seg.query_all();
        last_y = e.y;
    }

    cout << ans << '\n';
    return 0;
}
