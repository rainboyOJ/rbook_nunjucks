#include <bits/stdc++.h>
using namespace std;

vector<int> build_prefix_function(const string &pattern) {
    int m = (int)pattern.size();
    vector<int> pi(m, 0);

    for (int i = 1; i < m; i++) {
        int j = pi[i - 1];
        while (j > 0 && pattern[i] != pattern[j]) {
            j = pi[j - 1];
        }
        if (pattern[i] == pattern[j]) j++;
        pi[i] = j;
    }

    return pi;
}

vector<int> kmp_match(const string &text, const string &pattern) {
    vector<int> positions;
    int n = (int)text.size();
    int m = (int)pattern.size();

    if (m == 0) return positions;

    vector<int> pi = build_prefix_function(pattern);
    int j = 0;

    for (int i = 0; i < n; i++) {
        while (j > 0 && text[i] != pattern[j]) {
            j = pi[j - 1];
        }
        if (text[i] == pattern[j]) j++;

        if (j == m) {
            positions.push_back(i - m + 1);
            j = pi[j - 1];
        }
    }

    return positions;
}

int main() {
    string text, pattern;
    cin >> text >> pattern;

    vector<int> positions = kmp_match(text, pattern);
    if (positions.empty()) {
        cout << -1 << "\n";
        return 0;
    }

    for (int i = 0; i < (int)positions.size(); i++) {
        if (i) cout << ' ';
        cout << positions[i];
    }
    cout << "\n";

    return 0;
}
