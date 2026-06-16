#include <bits/stdc++.h>
using namespace std;

// Given a[0..n-1], find max a[i] + a[j] with i < j and j - i <= k.
// Monotonic deque keeps candidate i with maximum a[i] in the current window.

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n, k;
    cin >> n >> k;
    vector<long long> a(n);
    for (long long &x : a) cin >> x;

    deque<int> q;
    long long ans = LLONG_MIN;

    for (int j = 0; j < n; ++j) {
        while (!q.empty() && q.front() < j - k) q.pop_front();

        if (!q.empty()) {
            ans = max(ans, a[q.front()] + a[j]);
        }

        while (!q.empty() && a[q.back()] <= a[j]) q.pop_back();
        q.push_back(j);
    }

    cout << ans << '\n';
    return 0;
}
