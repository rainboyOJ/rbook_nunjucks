#include <bits/stdc++.h>
using namespace std;

int main() {
    int n;
    cin >> n;

    vector<int> a(n + 1), ans(n + 1, 0);
    for (int i = 1; i <= n; i++) {
        cin >> a[i];
    }

    // 栈中保存还没有找到右侧第一个更大元素的位置。
    vector<int> st;
    for (int i = 1; i <= n; i++) {
        while (!st.empty() && a[st.back()] < a[i]) {
            ans[st.back()] = i;
            st.pop_back();
        }
        st.push_back(i);
    }

    for (int i = 1; i <= n; i++) {
        cout << ans[i] << (i == n ? '\n' : ' ');
    }
    return 0;
}
