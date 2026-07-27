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

    vector<pair<int, int>> groups;
    for (int i = 0; i <= n;) {
        int j = i + 1;
        while (j <= n && prefix[j].value == prefix[i].value) ++j;
        groups.push_back({i, j});
        i = j;
    }

    if (target == 0) {
        for (auto [l, r] : groups) {
            for (int i = l; i < r; ++i) {
                for (int j = i + 1; j < r; ++j) {
                    int left = min(prefix[i].id, prefix[j].id) + 1;
                    int right = max(prefix[i].id, prefix[j].id);
                    cout << left << ' ' << right << '\n';
                }
            }
        }
        return 0;
    }

    int right_group = 0;
    for (int left_group = 0; left_group < (int)groups.size(); ++left_group) {
        right_group = max(right_group, left_group + 1);
        while (right_group < (int)groups.size()) {
            long long diff = prefix[groups[right_group].first].value
                           - prefix[groups[left_group].first].value;
            if (diff >= target) break;
            ++right_group;
        }
        if (right_group == (int)groups.size()) break;

        long long diff = prefix[groups[right_group].first].value
                       - prefix[groups[left_group].first].value;
        if (diff != target) continue;

        auto [l1, r1] = groups[left_group];
        auto [l2, r2] = groups[right_group];
        for (int i = l1; i < r1; ++i) {
            for (int j = l2; j < r2; ++j) {
                int left = min(prefix[i].id, prefix[j].id) + 1;
                int right = max(prefix[i].id, prefix[j].id);
                cout << left << ' ' << right << '\n';
            }
        }
    }

    return 0;
}
