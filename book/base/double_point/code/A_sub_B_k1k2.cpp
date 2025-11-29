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

    int cnt=0;
    int j1 =1 ,j2 = 1;
    for(int i = 1;i <= n;i++) {
        // 找到第一满足 >= k1条件的j1
        // 第一个不满足 < k1 条件的j1
        while( a[j1] - a[i] < k1  && j1 <=n ) j1++;

        // 找到最后一个满足条件的j2
        // 前一个位置 j2-1 就是最后一个满足 <= k2 位置
        while( a[j2] - a[i] <= k2 && j2 <=n ) j2++;
        if( j1 <= n  && j2 >= j1 ) {
            // ? (j2-1)-j1 + 1 = j2-j1
            // [j1,j2) 就是满足的区间
            cnt += ( j2 - j1);
        }
    }
    std::cout << cnt << "\n";
    return 0;
}
