#include <bits/stdc++.h>
using namespace std;

int main() {
    int n;
    cin >> n;

    vector<int> tail;
    for (int i = 0; i < n; i++) {
        int x;
        cin >> x;

        // tail[len - 1]: 长度为 len 的严格上升子序列中，末尾元素的最小可能值。
        auto it = lower_bound(tail.begin(), tail.end(), x);
        if (it == tail.end()) {
            tail.push_back(x);
        } else {
            *it = x;
        }
    }

    cout << tail.size() << "\n";
    return 0;
}
