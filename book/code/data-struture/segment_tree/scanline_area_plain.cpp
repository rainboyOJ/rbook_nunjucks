#include <bits/stdc++.h>
using namespace std;
using ll = long long;

// 扫描线线段树：维护区间被完整覆盖的次数与覆盖长度。
// 这个结构不需要懒标记下传：cover 只挂在整段被覆盖的节点上，
// 每个节点的覆盖长度由自己的 cover 和孩子信息当场算出。
const int maxn = 100000 + 5;  // 坐标段数上限，按题目规模调整

int cover[maxn << 2];  // cover[p]：节点 p 管辖区间被完整覆盖的次数
int len[maxn << 2];    // len[p]：节点 p 管辖区间内被覆盖的长度

int ls(int p) { return p << 1; }     // p 的左儿子 2p
int rs(int p) { return p << 1 | 1; } // p 的右儿子 2p+1

// 上推：自己的 cover > 0 说明整段被覆盖；否则由两个孩子拼起来
// 叶子且 cover == 0 时长度是 0，必须单独处理，否则会去读不存在孩子
void pushup(int p, int l, int r) {
    if (cover[p] > 0) len[p] = r - l + 1;
    else if (l == r) len[p] = 0;
    else len[p] = len[ls(p)] + len[rs(p)];
}

// 区间加覆盖次数：给 [ql, qr] 增加 v（v = +1 表示入边，-1 表示出边）
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

    // 每个矩形拆成入边和出边两个事件，按 y 排序后从上往下扫
    struct Event {
        int x1, x2, y, delta;
    };
    vector<Event> events;
    events.reserve(2 * n);

    int max_x = 0;
    for (int i = 0; i < n; i++) {
        int x1, y1, x2, y2;
        cin >> x1 >> y1 >> x2 >> y2;
        events.push_back({x1, x2 - 1, y1, 1});
        events.push_back({x1, x2 - 1, y2, -1});
        max_x = max(max_x, x2 - 1);
    }

    sort(events.begin(), events.end(),
         [](const Event &a, const Event &b) { return a.y < b.y; });

    int xn = max_x + 2;
    ll ans = 0;
    int last_y = events.empty() ? 0 : events[0].y;
    int last_len = 0;

    for (const Event &e : events) {
        ans += 1LL * last_len * (e.y - last_y);  // 上一层的高度先结账
        if (e.x1 <= e.x2) add(e.x1, e.x2, e.delta, 1, 1, xn);
        last_len = len[1];  // 根节点的覆盖长度就是当前这一层的高度
        last_y = e.y;
    }

    cout << ans << '\n';
    return 0;
}
