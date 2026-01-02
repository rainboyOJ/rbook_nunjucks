#include <bits/stdc++.h>
using namespace std;

const int maxn = 1e6 + 5;
int a[maxn];
int n;

void quick_sort(int l, int r) {
    // 1. 递归出口：如果区间只有一个数或没有数，直接返回
    if (l >= r) return;

    // 2. 选取基准数 (Pivot)
    // 建议取中间的数，防止在原本有序的数组上退化成 O(N^2)
    int mid = a[(l + r) / 2];
    
    // 定义双指针
    int i = l, j = r;

    // 3. Partition 分区操作
    // 目标：让左边 [l, j] 所有的数 <= mid
    //       让右边 [i, r] 所有的数 >= mid
    while (i <= j) {
        
        // 左指针向右找，直到找到一个 >= mid 的数停下
        // 注意：这里必须是 < mid，不能是 <=。遇到等于 mid 的也要停下，
        // 这样可以将重复的 mid 均匀分散到两边，避免树倾斜。
        while (a[i] < mid) i++;

        // 右指针向左找，直到找到一个 <= mid 的数停下
        while (a[j] > mid) j--;

        // 如果指针没有交错，说明找到了一对“放错位置”的数，交换它们
        if (i <= j) {
            swap(a[i], a[j]);
            i++;
            j--;
        }
    }

    // 4. 递归处理子区间
    // 此时指针已经“错车”了：j 在左边，i 在右边 (j < i)
    // 分割点变成了 j 和 i
    
    // 递归处理左半段 [l ... j]
    if (l < j) quick_sort(l, j);
    
    // 递归处理右半段 [i ... r]
    if (i < r) quick_sort(i, r);
}

int main() {
    // 读写加速
    ios::sync_with_stdio(false);
    cin.tie(0);

    cin >> n;
    for (int i = 1; i <= n; ++i) cin >> a[i];

    quick_sort(1, n);

    for (int i = 1; i <= n; ++i) cout << a[i] << " ";
    cout << endl;

    return 0;
}