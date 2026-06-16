#include <bits/stdc++.h>
using namespace std;

// 把二进制字符串转成十进制整数。
// 例: "1101" -> 13
long long bin2dec(const string& s) {
    long long ans = 0;
    for (char ch : s) {
        ans = ans * 2 + (ch - '0');
    }
    return ans;
}

int main() {
    string s;
    cin >> s;
    cout << bin2dec(s) << "\n";
    return 0;
}
