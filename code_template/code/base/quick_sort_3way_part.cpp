#include <bits/stdc++.h>
using namespace std;

const int maxn = 1e6+5;
int a[maxn];
int n;

// 三路快速排序 3-Way Quick Sort
void quick_sort(int l, int r) {
    if (l >= r) return;

    // 1. 随机选一个基准数 (防止被针对卡成 O(N^2))
    // 也可以简单写成 int key = a[(l+r)/2];
    int rand_idx = l + rand() % (r - l + 1);
    swap(a[l], a[rand_idx]); 
    int key = a[l]; // 选取 a[l] 作为 key，并以此展开

    // 2. 定义指针
    int lt = l;     // lt (less than) : 指向 "等于区" 的第一个位置
    int gt = r;     // gt (greater than): 指向 "等于区" 的最后一个位置
    int i = l + 1;  // i : 当前扫描到的位置 (从 l+1 开始，因为 a[l] 是 key)

    // 3. 扫描并分类
    while (i <= gt) {
        if (a[i] < key) {
            // 情况A: 发现比 key 小的，扔到左边 (lt)
            // 把 a[i] 和 a[lt] 交换，然后 lt 和 i 都右移
            swap(a[i], a[lt]);
            lt++;
            i++;
        } 
        else if (a[i] > key) {
            // 情况B: 发现比 key 大的，扔到右边 (gt)
            // 把 a[i] 和 a[gt] 交换，gt 左移
            // 注意：i 不能动！因为从 gt 换回来的数还没检查过
            swap(a[i], a[gt]);
            gt--;
        } 
        else {
            // 情况C: 等于 key，直接跳过，i 右移
            i++;
        }
    }

    // 此时数组状态：
    // [l ... lt-1] 都是 < key
    // [lt ... gt]  都是 == key (这一段已经排好了，不需要递归！)
    // [gt+1 ... r] 都是 > key

    // 4. 递归处理左右两边
    quick_sort(l, lt - 1);
    quick_sort(gt + 1, r);
}

int main() {
    // 基础输入输出优化
    ios::sync_with_stdio(false); cin.tie(0);
    
    cin >> n;
    for(int i = 1; i <= n; ++i) cin >> a[i];

    // 种子随机数，防止被黑客数据卡死
    srand(time(0)); 
    
    quick_sort(1, n);

    for(int i = 1; i <= n; ++i) cout << a[i] << (i == n ? "" : " ");
    cout << "\n";

    return 0;
}