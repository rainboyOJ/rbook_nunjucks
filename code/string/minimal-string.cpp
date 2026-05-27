// 2025-11-23 代码 我虽然凭直觉写了出来
// 但是我不能进行数学证明,我无法理解这个代码
// 我甚至无法证明这个算法是O(n)的
#include <bits/stdc++.h>
using namespace std;
const int maxn  = 2e7+5;

int n;;
char a[maxn];

int get_min(){
    int min_pos = 0;
    int i = 1;
    //用来比较的串
    while(i < n)
    {
        int k;
        for(k = 0;k < n;k++) {
            if( a[min_pos+k] != a[i+k]) break;
        }
        // if( k == n) break; // 周期串
        if( a[min_pos+k] < a[i+k]) { // min_pos 更小
            i = i+k+1; // 排除不可能的位置
        }
        else  { // i 更小
            // 计算下一个可能的位置
            int next_pos_1 =  min_pos +k+1;
            int next_pos_2 = i+1;

            // 更新min_pos
            min_pos = i;
            i = std::max(next_pos_1,next_pos_2);
        }
    }
    return min_pos;

}

int main (int argc, char *argv[]) {
    std::cin >> n;
    std::cin >> a;

    //复制字符串
    for(int i = 0;i < n ;++i ) // i: 0->n
        a[i+n] = a[i];
    int p = get_min();

    for(int i = 1;i <= n ;++i ) // i: 1->n
    {
        int tp = p+i-1;
        cout << a[tp % n] ;
    }
    return 0;
}