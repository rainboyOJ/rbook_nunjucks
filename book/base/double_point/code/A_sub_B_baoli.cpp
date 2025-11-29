#include <bits/stdc++.h>
using namespace std;

int n;
int a[10000];
int k1,k2;

int main (int argc, char *argv[]) {
    std::cin >> n;
    for(int i = 1;i <= n ;++i ) // i: 1->n
    {
        std::cin >> a[i];
    }
    std::cin >> k1 >> k2;
    std::sort(a+1,a+1+n);

    int ans = 0;
    for(int i = 1;i <= n ;++i ) // i: 1->n
    {
        for(int j = i;j <= n ;++j ) // j: 1->n
        {
            int t = a[j] - a[i];
            if( t >= k1 && t <= k2)
                ans++;
        }
    }
    std::cout << ans << "\n";
    return 0;
}
