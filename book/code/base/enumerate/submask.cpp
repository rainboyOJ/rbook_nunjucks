#include <bits/stdc++.h>
using namespace std;

// 返回 mask 的所有非空子集。
// 这里的子集指二进制意义上的子掩码：sub 的 1 只能出现在 mask 为 1 的位置。
vector<int> non_empty_submasks(int mask) {
    vector<int> res;
    for (int sub = mask; sub; sub = (sub - 1) & mask) {
        res.push_back(sub);
    }
    return res;
}

void print_bits(int x, int n) {
    for (int i = n - 1; i >= 0; --i) {
        cout << ((x >> i) & 1);
    }
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n, mask;
    cin >> n >> mask;

    for (int sub : non_empty_submasks(mask)) {
        cout << sub << ' ';
        print_bits(sub, n);
        cout << '\n';
    }

    // 如果需要枚举空集，在循环后单独处理 0 即可。
    return 0;
}
