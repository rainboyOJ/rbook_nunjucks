#include <bits/stdc++.h>
using namespace std;

struct Prefix {
    long long value;
    int id;

    bool operator<(const Prefix& other) const {
        if (value != other.value) return value < other.value;
        return id < other.id;
    }
};

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n;
    long long target;
    cin >> n;

    vector<Prefix> prefix(n + 1);
    prefix[0] = {0, 0};

    for (int i = 1; i <= n; ++i) {
        long long x;
        cin >> x;
        prefix[i] = {prefix[i - 1].value + x, i};
    }
    cin >> target;

    sort(prefix.begin(), prefix.end());

    for (int j = 1; j <= n; ++j) {
        for (int i = 0; i < j; ++i) {
            if (prefix[j].value - prefix[i].value == target) {
                int l = min(prefix[i].id, prefix[j].id) + 1;
                int r = max(prefix[i].id, prefix[j].id);
                cout << l << ' ' << r << '\n';
            }
        }
    }

    return 0;
}
