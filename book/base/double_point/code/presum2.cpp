#include <iostream>
#include <utility>
using namespace std;
int n;
int m;

struct Node {
    int val,id;
    bool operator<(const Node &t)const {
        return val < t.val;
    }
};
Node p[100009];

void work(){
    int i = 0,j = 1;
    while( j <= n){
        int sum = p[j].val - p[i].val;
        if( sum == m) {
            int t1 = p[j].id;
            int t2 = p[i].id + 1;
            if( t1 > t2) std::swap(t1,t2);
            cout << t1 << " " << t2 << endl;
            i++;
        }
        else if( sum > m) i++;
        else if( sum < m) j++;
    }
}

int main() {
    std::cin >> n;
    for(int i = 1;i <= n ;++i ) // i: 0->n
    {
        int t;
        std::cin >> t;
        p[i] = {p[i-1].val +t ,i};
    }
    std::cin >> m;
    sort(p,p+1+n); //排序
    work();
    return 0;
}
