#include <bits/stdc++.h>
using namespace std;

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n, target;
    cin >> n >> target;

    vector<int> a(n);
    for (int i = 0; i < n; i++) {
        cin >> a[i];
    }

    int ans = n + 1;
    int sum = 0;
    int left = 0;

    for (int right = 0; right < n; right++) {
        sum += a[right];
        while (sum >= target) {
            ans = min(ans, right - left + 1);
            sum -= a[left];
            left++;
        }
    }

    cout << (ans == n + 1 ? 0 : ans) << '\n';
    return 0;
}
