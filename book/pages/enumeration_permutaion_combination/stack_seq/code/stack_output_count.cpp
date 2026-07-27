#include <bits/stdc++.h>
using namespace std;

// Count possible output sequences of stack operations.
// State:
//   in_left: how many numbers are still waiting to be pushed
//   in_stack: how many numbers are currently in the stack
//
// From one state:
//   push: if in_left > 0, move one waiting number into the stack
//   pop : if in_stack > 0, pop one number into the output sequence

using int64 = long long;

int n;
int64 memo[25][25];
bool vis[25][25];

int64 dfs(int in_left, int in_stack) {
    if (in_left == 0) return 1;
    if (vis[in_left][in_stack]) return memo[in_left][in_stack];
    vis[in_left][in_stack] = true;

    int64 ans = 0;
    if (in_left > 0) {
        ans += dfs(in_left - 1, in_stack + 1);
    }
    if (in_stack > 0) {
        ans += dfs(in_left, in_stack - 1);
    }

    memo[in_left][in_stack] = ans;
    return ans;
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    cin >> n;
    cout << dfs(n, 0) << '\n';
    return 0;
}
