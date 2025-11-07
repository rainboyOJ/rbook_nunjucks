/**
 * Author by {{author}} blog: {{blog}} github : {{github}}
 * date: {{date}}
 * oj: {{oj}}
 * title: {{title}}
 * description: {{description}}
 */
#include <bits/stdc++.h>
using namespace std;

typedef long long ll;

// #define NO_DEBUG // switch debug
#if defined(onlinejudge) || defined(ONLINE_JUDGE) || defined(NO_DEBUG)
#define log(...)
#define fenc
#else
#define log(args...) { cout << "LINE:" << __LINE__ << " : ";string _s = #args; replace(_s.begin(), _s.end(), ',', ' '); stringstream _ss(_s); istream_iterator<string> _it(_ss); err(_it, args); }
#define fenc cout<<"================================";
void err(istream_iterator<string> it) {}

template<typename T>
void err(istream_iterator<string> it, T a) {
cerr << *it << " = " << a << "\n";
}

template<typename T, typename... Args>
void err(istream_iterator<string> it, T a, Args... args) {
cerr << *it << " = " << a << ", ";
err(++it, args...);
}
#endif
const int maxn = 1e6+5;
int n,m;
int a[maxn];

{{include "code/utils/quick_io.cpp"}}

void init() {
    read(n);
    CURRENT_LINE


}

signed main () {
#ifdef FREOPEN
    freopen("in", "r",stdin);
#endif
    // std::ios::sync_with_stdio(false); cin.tie(0); cout.tie(0); // 关闭io同步


    return 0;
}
