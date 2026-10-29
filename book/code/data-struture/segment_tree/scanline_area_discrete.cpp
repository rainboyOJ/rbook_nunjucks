#include <bits/stdc++.h>
using namespace std;
using ll = long long;

// 扫描线线段树（离散化版）：坐标很大时先把 x 坐标压成 1..n 的段号。
// 与朴素版一样，cover 只挂在整段被覆盖的节点上，不需要懒标记下传。
const int maxn = 100000 + 5;  // 坐标段数上限，按题目规模调整

int cover[maxn << 2];  // cover[p]：节点 p 管辖区间被完整覆盖的次数
ll len[maxn << 2];     // len[p]：节点 p 管辖区间内被覆盖的实际长度

int xs[maxn];  // xs[1..n+1]：离散化后的 x 坐标，第 i 段是 [xs[i], xs[i+1])

int ls(int p) { return p << 1; }     // p 的左儿子 2p
int rs(int p) { return p << 1 | 1; } // p 的右儿子 2p+1

// 上推：自己的 cover > 0 说明整段被覆盖，长度按坐标差算；
// 否则由两个孩子拼起来。叶子且 cover == 0 时长度是 0。
void pushup(int p, int l, int r) {
    if (cover[p] > 0) len[p] = xs[r + 1] - xs[l];
    else if (l == r) len[p] = 0;
    else len[p] = len[ls(p)] + len[rs(p)];
}

// 区间加覆盖次数：给第 [ql, qr] 段增加 v（v = +1 表示入边，-1 表示出边）
void add(int ql, int qr, int v, int p, int l, int r) {
    if (qr < l || r < ql) return;
    if (ql <= l && r <= qr) {
        cover[p] += v;
        pushup(p, l, r);
        return;
    }
    int mid = (l + r) >> 1;
    add(ql, qr, v, ls(p), l, mid);
    add(ql, qr, v, rs(p), mid + 1, r);
    pushup(p, l, r);
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n;
    cin >> n;

    struct Event {
        int x1, x2, y, delta;
    };
    vector<Event> events;
    events.reserve(2 * n);
    vector<int> xv;
    xv.reserve(2 * n);

    for (int i = 0; i < n; i++) {
        int x1, y1, x2, y2;
        cin >> x1 >> y1 >> x2 >> y2;
        events.push_back({x1, x2, y1, 1});
        events.push_back({x1, x2, y2, -1});
        xv.push_back(x1);
        xv.push_back(x2);
    }

    sort(events.begin(), events.end(),
         [](const Event &a, const Event &b) { return a.y < b.y; });

    // 离散化：把 x 坐标压成连续的段号
    sort(xv.begin(), xv.end());
    xv.erase(unique(xv.begin(), xv.end()), xv.end());
    int cnt = (int)xv.size();
    for (int i = 0; i < cnt; i++) xs[i + 1] = xv[i];

    int segn = cnt - 1;  // 一共 segn 段：[xs[i], xs[i+1])
    if (segn <= 0) {
        cout << 0 << '\n';
        return 0;
    }

    ll ans = 0;
    int last_y = events[0].y;
    ll last_len = 0;

    for (const Event &e : events) {
        ans += last_len * (e.y - last_y);
        // 左闭右开：覆盖 [x1, x2) 就是第 [rank(x1), rank(x2)-1] 段
        int l = lower_bound(xv.begin(), xv.end(), e.x1) - xv.begin() + 1;
        int r = lower_bound(xv.begin(), xv.end(), e.x2) - xv.begin();
        if (l <= r) add(l, r, e.delta, 1, 1, segn);
        last_len = len[1];
        last_y = e.y;
    }

    cout << ans << '\n';
    return 0;
}
