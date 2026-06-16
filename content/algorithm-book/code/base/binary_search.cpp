//这是最快的写法
int mid(int l,int r) { return (l+r) >> 1; }

//检查pos位置的值是否符合要求
bool check(int pos,int val){
    return a[pos] >= val;
}

//bs_find = binary search find
// 返回第一个满足条件的位置
// 如果所有的值都不满足条件,返回r, 
// !!! r位置对应的是Guard,一个虚拟的位置,保证一定各满足check
// !!! 写题目的时候一定要注意r的值,通常是n+1
// 保障初始区间[l,r],r一定是满足的,通常r = n+1
int bs_find(int l,int r,int val) {
    while( l < r) {
        int m = mid(l,r);
        if( check(m,val)) //成立
            r = m;
        else //不成立,抛弃左半边
            l = m+1;
    }
    return l ;
}