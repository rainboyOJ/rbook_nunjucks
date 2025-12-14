#include <bits/stdc++.h>
using namespace std;

const int maxn = 1e5+5;
bool del[maxn]; // 0 表示没有删除
vector<int> prime;

//埃式筛法
/* 原理: 
 *  - 2是最小的的素数,2的k倍都不是素数,k>=2
 *  - 下一个没有被筛掉的数是3,所以3是素数(原理:合数一定可以拆出一个小于自己的素数因子)
 *      - 删除3的k>=3倍数
 *  - 4 被删除,不用岀它的倍数
 *  - ....
 * */
void E_prime(int n){
    //memset(del,0,sizeof(del));
    for(int i=2;i<=n;i++){
        if( del[i] == 0){
            prime.push_back(i);
            if( i > n / i ) continue; //防溢出
            for(int j=i*i;j<=n;j+=i) del[j] = 1;
        }
    }
}

int main(){
    E_prime(100); // 求100内的素数
    for (const auto& e : prime) {
        cout << e << " ";
    }
    cout << endl;
    return 0;
}