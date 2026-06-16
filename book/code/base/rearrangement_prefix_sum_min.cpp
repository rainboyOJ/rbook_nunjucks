#include <algorithm>
#include <iostream>
#include <vector>
using namespace std;

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n;
    cin >> n;

    vector<long long> a(n);
    for (int i = 0; i < n; ++i) cin >> a[i];

    sort(a.begin(), a.end());

    long long prefix = 0;
    long long answer = 0;
    for (long long x : a) {
        prefix += x;
        answer += prefix;
    }

    cout << answer << '\n';
    return 0;
}
