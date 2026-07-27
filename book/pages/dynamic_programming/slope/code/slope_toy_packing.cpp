#include <algorithm>
#include <deque>
#include <iostream>
#include <string>
#include <vector>
using namespace std;

using i128 = __int128_t;

string to_string_i128(i128 x) {
    if (x == 0) return "0";
    bool negative = x < 0;
    if (negative) x = -x;

    string s;
    while (x > 0) {
        s.push_back(char('0' + x % 10));
        x /= 10;
    }
    if (negative) s.push_back('-');
    reverse(s.begin(), s.end());
    return s;
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n;
    long long L;
    cin >> n >> L;

    vector<long long> prefix(n + 1, 0);
    for (int i = 1; i <= n; ++i) {
        long long c;
        cin >> c;
        prefix[i] = prefix[i - 1] + c;
    }

    vector<long long> x(n + 1, 0);
    vector<i128> dp(n + 1, 0);
    for (int i = 1; i <= n; ++i) {
        x[i] = prefix[i] + i;
    }

    auto b = [&](int i) -> long long {
        return x[i] - L - 1;
    };

    auto y = [&](int j) -> i128 {
        return dp[j] + (i128)x[j] * x[j];
    };

    auto value = [&](int j, int i) -> i128 {
        return y(j) - (i128)2 * b(i) * x[j];
    };

    auto bad = [&](int a, int mid, int c) -> bool {
        return (y(mid) - y(a)) * (x[c] - x[mid]) >=
               (y(c) - y(mid)) * (x[mid] - x[a]);
    };

    deque<int> q;
    q.push_back(0);

    for (int i = 1; i <= n; ++i) {
        while (q.size() >= 2 && value(q[1], i) <= value(q[0], i)) {
            q.pop_front();
        }

        int j = q.front();
        dp[i] = (i128)b(i) * b(i) + value(j, i);

        while (q.size() >= 2 && bad(q[q.size() - 2], q[q.size() - 1], i)) {
            q.pop_back();
        }
        q.push_back(i);
    }

    cout << to_string_i128(dp[n]) << '\n';
    return 0;
}
