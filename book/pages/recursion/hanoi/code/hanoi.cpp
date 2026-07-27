#include <bits/stdc++.h>
using namespace std;

// 把 n 个盘子从 from 移到 to，借助 mid。
long long hanoi(int n, char from, char mid, char to) {
    if (n == 0) return 0;

    long long moves = 0;
    moves += hanoi(n - 1, from, to, mid);
    cout << from << "->" << to << '\n';
    moves += 1;
    moves += hanoi(n - 1, mid, from, to);
    return moves;
}

int main() {
    int n;
    cin >> n;
    cout << hanoi(n, 'A', 'B', 'C') << '\n';
    return 0;
}
