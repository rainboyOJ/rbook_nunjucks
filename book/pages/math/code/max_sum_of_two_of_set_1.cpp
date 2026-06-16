int n;
int a[maxn];

int max1 = -inf,max2=-inf;
void upd_max_2(int t) {
    if( t > max1 ) {
        max2 = max1;
        max1 = t;
    } else if( t > max2 ) {
        max2 = t;
    }
}

for(int i =1;i<=n;i++) {
    upd_max_2(a[i]);
}

cout << max1 + max2 << endl;