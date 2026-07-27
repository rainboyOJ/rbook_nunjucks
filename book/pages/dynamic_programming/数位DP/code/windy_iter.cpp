#include <bits/stdc++.h>
using namespace std;

// Iterative digit DP for Windy numbers.
// f[len][first_digit] = number of len-digit suffixes whose highest digit is
// first_digit and every adjacent digit differs by at least 2.

int f[12][10];

void init() {
    for (int d = 0; d <= 9; ++d) f[1][d] = 1;

    for (int len = 2; len <= 11; ++len) {
        for (int first = 0; first <= 9; ++first) {
            for (int next = 0; next <= 9; ++next) {
                if (abs(first - next) >= 2) {
                    f[len][first] += f[len - 1][next];
                }
            }
        }
    }
}

int calc(int n) {
    if (n <= 0) return 0;

    vector<int> digit(1, 0);
    while (n > 0) {
        digit.push_back(n % 10);
        n /= 10;
    }

    int len = (int)digit.size() - 1;
    int res = 0;

    for (int l = 1; l < len; ++l) {
        for (int first = 1; first <= 9; ++first) {
            res += f[l][first];
        }
    }

    for (int first = 1; first < digit[len]; ++first) {
        res += f[len][first];
    }

    for (int pos = len - 1; pos >= 1; --pos) {
        for (int cur = 0; cur < digit[pos]; ++cur) {
            if (abs(cur - digit[pos + 1]) >= 2) {
                res += f[pos][cur];
            }
        }

        if (abs(digit[pos] - digit[pos + 1]) < 2) break;
        if (pos == 1) ++res;
    }

    return res;
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    init();

    int l, r;
    cin >> l >> r;
    cout << calc(r) - calc(l - 1) << '\n';
    return 0;
}
