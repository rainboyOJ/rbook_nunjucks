#include <bits/stdc++.h>
using namespace std;

// 把非负十进制整数转成二进制字符串。
// 例: 13 -> "1101"
string dec2bin(long long n) {
    if (n == 0) return "0";

    string ans;
    while (n > 0) {
        ans.push_back(char('0' + n % 2));
        n /= 2;
    }
    reverse(ans.begin(), ans.end());
    return ans;
}

int main() {
    long long n;
    cin >> n;
    cout << dec2bin(n) << "\n";
    return 0;
}
