#include <bits/stdc++.h>
using namespace std;

int n;

// dep 表示当前递归深度，也就是正在处理的数字。
void print_num(int dep) {
    if (dep > n) {
        cout << '\n';
        return;
    }

    cout << dep << ' ';   // 递归前进阶段
    print_num(dep + 1);
    cout << dep << ' ';   // 递归回溯阶段
}

int main() {
    cin >> n;
    print_num(1);
    return 0;
}
