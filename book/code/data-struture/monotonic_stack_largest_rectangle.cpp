#include <bits/stdc++.h>
using namespace std;

int main() {
    while (true) {
        int n;
        cin >> n;
        if (n == 0) break;

        vector<long long> h(n + 2, 0);
        for (int i = 1; i <= n; i++) {
            cin >> h[i];
        }

        long long ans = 0;
        vector<int> st;
        st.push_back(0);

        for (int i = 1; i <= n + 1; i++) {
            while (!st.empty() && h[st.back()] > h[i]) {
                int mid = st.back();
                st.pop_back();
                long long height = h[mid];
                long long width = i - st.back() - 1;
                ans = max(ans, height * width);
            }
            st.push_back(i);
        }

        cout << ans << "\n";
    }
    return 0;
}
