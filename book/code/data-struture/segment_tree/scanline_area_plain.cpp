#include <bits/stdc++.h>
using namespace std;

using i64 = long long;

struct Event {
    int x1, x2, y, delta;
    bool operator<(const Event& other) const {
        return y < other.y;
    }
};

struct SegmentTree {
    vector<int> cover, covered_len;

    explicit SegmentTree(int n) : cover(n * 4 + 5), covered_len(n * 4 + 5) {}

    void pull(int p, int l, int r) {
        if (cover[p] > 0) {
            covered_len[p] = r - l + 1;
        } else if (l == r) {
            covered_len[p] = 0;
        } else {
            covered_len[p] = covered_len[p << 1] + covered_len[p << 1 | 1];
        }
    }

    void add(int ql, int qr, int v, int p, int l, int r) {
        if (ql <= l && r <= qr) {
            cover[p] += v;
            pull(p, l, r);
            return;
        }
        int mid = (l + r) >> 1;
        if (ql <= mid) add(ql, qr, v, p << 1, l, mid);
        if (mid < qr) add(ql, qr, v, p << 1 | 1, mid + 1, r);
        pull(p, l, r);
    }

    int query_all() const {
        return covered_len[1];
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
    SegmentTree seg(max_x + 2);

    i64 ans = 0;
    int last_y = events.empty() ? 0 : events[0].y;
    int last_len = 0;

    for (const auto& e : events) {
        ans += 1LL * last_len * (e.y - last_y);
        if (e.x1 <= e.x2) seg.add(e.x1, e.x2, e.delta, 1, 1, max_x);
        last_len = seg.query_all();
        last_y = e.y;
    }

    cout << ans << '\n';
    return 0;
}
