#include <bits/stdc++.h>
#include <climits>
using namespace std;

int n,m;
int a[100];
int ans = INT_MAX;


int main() {

    std::cin >> n;
    for(int i = 1;i <= n ;++i ) // i: 1->n
    {
        std::cin >> a[i];
    }
    std::cin >> m;

    int i = 1;
    int sum = 0;
    for(int j = 1;j <= n ;++j ) // 结尾j
    {
        sum += a[j];
        while( sum >= m) {
            ans = std::min(ans,j-i+1);
            sum -=a[i];
            i++;
        }
    }
    cout << (ans == INT_MAX ? 0 : ans) <<endl;
    return 0;
}