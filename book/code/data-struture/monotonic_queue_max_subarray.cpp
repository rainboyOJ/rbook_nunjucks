#include <bits/stdc++.h>
using namespace std;

int main() {
    int n, m;
    cin >> n >> m;

    vector<long long> prefix(n + 1, 0);
    for (int i = 1; i <= n; i++) {
        long long x;
        cin >> x;
        prefix[i] = prefix[i - 1] + x;
    }

    long long ans = LLONG_MIN;
    deque<int> q;
    q.push_back(0);

    for (int i = 1; i <= n; i++) {
        while (!q.empty() && q.front() < i - m) {
            q.pop_front();
        }

        ans = max(ans, prefix[i] - prefix[q.front()]);

        while (!q.empty() && prefix[q.back()] >= prefix[i]) {
            q.pop_back();
        }
        q.push_back(i);
    }

    cout << ans << "\n";
    return 0;
}
