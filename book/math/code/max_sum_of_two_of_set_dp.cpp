int a[maxn]; // 原始数据
int B[maxn]; // B[i] 表示以 a_i 和 前面的某个数a_j的最大和
int f = a[1]; // f 表示前面的某个数a_j的最大值
int ans;

template<typename T>
void upd(T& a, T b) { if( a < b ) a = b; }


for(int i =2;i<=n;i++) {
    B[i] = f + a[i];
    // 核心 两行代码
    upd(ans,f + a[i]); // 更新答案, 得到 a[i] 为结尾的分类的最大值
    upd(f,a[i]); // 更新 f, 为后面的 B[i] 服务
}