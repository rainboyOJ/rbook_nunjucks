#include <bits/stdc++.h>
using namespace std;

int main() {
    int n, k;
    cin >> n >> k;

    vector<int> a(n + 1);
    for (int i = 1; i <= n; i++) {
        cin >> a[i];
    }

    deque<int> q;
    for (int i = 1; i <= n; i++) {
        while (!q.empty() && q.front() <= i - k) {
            q.pop_front();
        }
        while (!q.empty() && a[q.back()] >= a[i]) {
            q.pop_back();
        }
        q.push_back(i);
        if (i >= k) {
            cout << a[q.front()] << (i == n ? '\n' : ' ');
        }
    }

    q.clear();
    for (int i = 1; i <= n; i++) {
        while (!q.empty() && q.front() <= i - k) {
            q.pop_front();
        }
        while (!q.empty() && a[q.back()] <= a[i]) {
            q.pop_back();
        }
        q.push_back(i);
        if (i >= k) {
            cout << a[q.front()] << (i == n ? '\n' : ' ');
        }
    }

    return 0;
}
