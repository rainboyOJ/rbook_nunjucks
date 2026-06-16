#include <bits/stdc++.h>
#include <climits>
using namespace std;

int n,m;
int p[100];
int ans = INT_MAX;

int range_sum(int l,int r) {
    return p[r] - p[l-1];
}


int main() {

    std::cin >> n;
    for(int i = 1;i <= n ;++i ) // i: 1->n
    {
        std::cin >> p[i];
        p[i] += p[i-1];
    }
    std::cin >> m;

    for(int j = 1;j <= n ;++j ) // 结尾j
    {
        for(int i = 1;i <= j ;++i ) // i: 1->j
        {
            int len = j-i+1;
            int sum = range_sum(i,j);
            if( sum >= m && len < ans)
            {
                // cout << i << " " << j << endl;
                ans = len;
            }
        }
    }
    cout << ans<<endl;
    return 0;
}
