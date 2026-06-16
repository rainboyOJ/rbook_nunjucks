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
    vector<int> cover;
    vector<i64> covered_len;
    const vector<int>* xs = nullptr;

    SegmentTree(int n, const vector<int>& coord)
        : cover(n * 4 + 5), covered_len(n * 4 + 5), xs(&coord) {}

    void pull(int p, int l, int r) {
        if (cover[p] > 0) {
            covered_len[p] = 1LL * (*xs)[r] - (*xs)[l - 1];
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

    i64 query_all() const {
        return covered_len[1];
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

    SegmentTree seg(segment_count, xs);
    i64 ans = 0;
    int last_y = events.empty() ? 0 : events[0].y;
    i64 last_len = 0;

    for (const auto& e : events) {
        ans += last_len * (e.y - last_y);

        int l = lower_bound(xs.begin(), xs.end(), e.x1) - xs.begin() + 1;
        int r = lower_bound(xs.begin(), xs.end(), e.x2) - xs.begin();
        if (l <= r) seg.add(l, r, e.delta, 1, 1, segment_count);

        last_len = seg.query_all();
        last_y = e.y;
    }

    cout << ans << '\n';
    return 0;
}
