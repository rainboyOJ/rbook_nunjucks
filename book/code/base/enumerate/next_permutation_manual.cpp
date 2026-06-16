#include <bits/stdc++.h>
using namespace std;

// Manual implementation of lexicographic next_permutation.
// It returns false when the current sequence is already the last permutation.
bool next_permutation_manual(vector<int> &a) {
    int n = (int)a.size();

    // 1. Find the rightmost position i with a[i] < a[i + 1].
    // The suffix a[i + 1..n - 1] is non-increasing.
    int i = n - 2;
    while (i >= 0 && a[i] >= a[i + 1]) --i;
    if (i < 0) return false;

    // 2. Find the rightmost element larger than a[i].
    int j = n - 1;
    while (a[j] <= a[i]) --j;

    // 3. Make the permutation slightly larger, then minimize the suffix.
    swap(a[i], a[j]);
    reverse(a.begin() + i + 1, a.end());
    return true;
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n;
    cin >> n;
    vector<int> a(n);
    for (int i = 0; i < n; ++i) a[i] = i + 1;

    do {
        for (int i = 0; i < n; ++i) {
            if (i) cout << ' ';
            cout << a[i];
        }
        cout << '\n';
    } while (next_permutation_manual(a));

    return 0;
}
