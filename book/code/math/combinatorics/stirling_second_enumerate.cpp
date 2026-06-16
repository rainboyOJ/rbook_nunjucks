#include <bits/stdc++.h>
using namespace std;

// Enumerate all partitions of n distinct balls into m non-empty identical boxes.
// To avoid duplicates caused by identical boxes, box ids are created in order:
// ball 1 must be in box 1; when processing a new ball, it may go into any
// existing box, or open exactly the next new box.

int n, m;
vector<vector<int>> box_items;
int answer_count = 0;

void print_answer() {
    cout << setw(4) << ++answer_count << ": ";
    for (int i = 0; i < m; ++i) {
        cout << "[ ";
        for (int x : box_items[i]) cout << x << ' ';
        cout << "] ";
    }
    cout << '\n';
}

void dfs(int ball, int used_boxes) {
    if (ball == n + 1) {
        if (used_boxes == m) print_answer();
        return;
    }

    // Put this ball into an existing box.
    for (int i = 0; i < used_boxes; ++i) {
        box_items[i].push_back(ball);
        dfs(ball + 1, used_boxes);
        box_items[i].pop_back();
    }

    // Open a new box. It must be the next box id, which gives each partition
    // one canonical representation.
    if (used_boxes < m) {
        box_items[used_boxes].push_back(ball);
        dfs(ball + 1, used_boxes + 1);
        box_items[used_boxes].pop_back();
    }
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    cin >> n >> m;
    if (n < m || m <= 0) {
        cout << 0 << '\n';
        return 0;
    }

    box_items.assign(m, {});
    box_items[0].push_back(1);
    dfs(2, 1);
    return 0;
}
