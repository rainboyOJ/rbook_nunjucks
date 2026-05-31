#include <bits/stdc++.h>
using namespace std;

int dp[15][20]; 
// dp[pos][last] 表示: 在不贴上界、没有前导零、上一位为 last 时,
// 还剩 pos 位可以填写的合法方案数。

int num[15]; // 数字的拆分

// 从高位到低位填写数字。
// pos: 当前还要填写第 pos 位, num[pos] 是这一位的上界数字。
// last: 上一位已经填的数字。
// limit: 前缀是否仍然贴着上界。
// lead: 前面是否全是前导零。
int dfs(int pos,int last,bool limit, bool lead) {
    // 全部位都填完后, 如果仍然全是前导零, 说明没有形成正整数。
    if( pos ==0 ) return lead ? 0 : 1;
    if( !limit && !lead  && dp[pos][last] != -1)
    {
        return dp[pos][last];
    }

    int up = limit ? num[pos] : 9; //上界
    
    int res = 0;

    for(int i = 0; i<= up;i++)
    {
        if( lead ) {
            res += dfs(pos-1,i,limit && (i == num[pos]), lead && (i == 0) );
        }
        else {
            if( abs(i - last) >=2)
                res += dfs(pos-1,i,limit && (i == num[pos]), lead && (i==0));
              // 这里 lead && (i==0) 一定是 false。
        }

    }

    if( !limit && !lead)
        dp[pos][last] = res;
    return res;


}

int solve(int x) {
    if( x == 0) return 0;
    int len = 0;
    while(x) {
        num[++len] = x % 10;
        x /= 10;
    }
    return dfs(len,0,true,true);
}

int main (int argc, char *argv[]) {

    memset(dp,-1,sizeof(dp));

        int l ,r;
        cin >> l >> r;
        cout << solve(r) - solve(l-1) << endl;
        

    
    return 0;
}
