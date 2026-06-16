#include <bits/stdc++.h>
using namespace std;

// [i, j) 的长度：包含 i，不包含 j。
int distance_exclude_right(int i, int j) {
    return j - i;
}

// [i, j] 的长度：同时包含 i 和 j。
int distance_include_right(int i, int j) {
    return j - i + 1;
}

struct RelativePosition {
    int pos;

    // 从当前位置向 dir 方向走 n 步，不把当前位置算作第 1 个位置。
    int move_exclude_current(int n, int dir) const {
        return pos + n * dir;
    }

    // 从当前位置向 dir 方向数 n 个位置，把当前位置算作第 1 个位置。
    int move_include_current(int n, int dir) const {
        return pos + (n - 1) * dir;
    }

    int next_exclude_current(int n) const {
        return move_exclude_current(n, 1);
    }

    int next_include_current(int n) const {
        return move_include_current(n, 1);
    }

    int prev_exclude_current(int n) const {
        return move_exclude_current(n, -1);
    }

    int prev_include_current(int n) const {
        return move_include_current(n, -1);
    }
};

int main() {
    cout << distance_exclude_right(3, 9) << '\n';
    cout << distance_include_right(3, 9) << '\n';

    RelativePosition p{10};
    cout << p.next_exclude_current(3) << '\n';
    cout << p.next_include_current(3) << '\n';
    return 0;
}
