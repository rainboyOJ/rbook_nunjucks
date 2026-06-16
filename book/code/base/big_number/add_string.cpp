#include <bits/stdc++.h>
using namespace std;

string add_positive_integer(string a, string b) {
    if (a.size() < b.size()) swap(a, b);
    reverse(a.begin(), a.end());
    reverse(b.begin(), b.end());

    int carry = 0;
    string ans;
    for (size_t i = 0; i < a.size(); ++i) {
        int x = a[i] - '0';
        int y = (i < b.size() ? b[i] - '0' : 0);
        int sum = x + y + carry;
        ans.push_back(char('0' + sum % 10));
        carry = sum / 10;
    }
    if (carry) ans.push_back(char('0' + carry));

    while (ans.size() > 1 && ans.back() == '0') ans.pop_back();
    reverse(ans.begin(), ans.end());
    return ans;
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    string a, b;
    cin >> a >> b;
    cout << add_positive_integer(a, b) << '\n';
    return 0;
}
